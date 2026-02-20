# Testing Locally

When developing locally, you'll need some way to test your changes. There are currently 
two approaches to testing locally:

 1. Automated NixOS tests: Run with `nix flake check`
 2. Manual testing: Run the services locally with a mock GitHub server

The automated tests are limited. They can only test the Hydra -> GitHub direction, because
building `github:xyz` flake URLs require network access, which is restricted in the NixOS
test runner. To run them, no further setup is required, just run `nix flake check`.

For more extensive testing, manual testing is required. To run all the required
components, follow the steps in the rest of this document.

## Prerequisites

Before getting started, ensure you have:

 * Nix
 * Direnv
 * Basic tools: `jq`, `openssl`, and `curl`

## Step 1: Install Hydra CI and PostgreSQL

On NixOS, Hydra and PostgreSQL can be installed with the NixOS module:

    hydra = {
      enable = true;
      hydraURL = "http://localhost:8000";
      port = 8000;
      notificationSender = "hydra@localhost";
      buildMachinesFiles = [];
      useSubstitutes = true;
    };

On other platforms, refer to the [Hydra manual](https://nixos.org/hydra/manual/).

## Step 2: Configure Hydra

Run the following SQL script on the Hydra database:

    CREATE TABLE IF NOT EXISTS github_commands (
        id SERIAL PRIMARY KEY,
        command JSONB NOT NULL,
        created TIMESTAMP DEFAULT NOW(),
        processed TIMESTAMP DEFAULT NULL
    );

    CREATE TABLE github_status (
        id SERIAL PRIMARY KEY,
        owner TEXT NOT NULL,
        repo TEXT NOT NULL,
        headSha TEXT NOT NULL,
        name TEXT NOT NULL,
        UNIQUE (owner, repo, headSha, name)
    );

    CREATE TABLE github_status_payload (
        id SERIAL PRIMARY KEY,
        status_id INTEGER NOT NULL, -- fk: github_status.id
        payload JSONB NOT NULL,
        created TIMESTAMP DEFAULT NOW(),
        sent TIMESTAMP DEFAULT NULL,
        tries INTEGER DEFAULT 0,
        FOREIGN KEY (status_id) REFERENCES github_status (id) ON DELETE CASCADE
    );

Then create a Hydra user:

    sudo -u hydra hydra-create-user bridge --password hydra --role admin

## Step 3: Configure Hydra Tools

Generate a fake GitHub application private key:

    openssl genrsa -out app.key

Add required environment variables to .envrc:

    export HYDRA_HOST=http://localhost:8000
    export HYDRA_API_USER=bridge
    export HYDRA_API_PASS=hydra
    export HYDRA_DB=localhost
    export HYDRA_DB_USER=hydra
    export HYDRA_DB_PASS=hydra
    export HYDRA_STATE_DIR=/var/lib/hydra
    export GITHUB_ENDPOINT_URL=http://localhost:4010
    export GITHUB_APP_ID=12345
    export GITHUB_APP_INSTALL_IDS="[(\"input-output-hk\", 1)]"
    export GITHUB_APP_KEY_FILE=$(pwd)/app.key
    export GITHUB_WEBHOOK_SECRET=TOPSECRET

## Step 4: Start Hydra Tools

Load .envrc into your shell:

    direnv allow

Start the mock GitHub server:

    mockoon-cli start --data mock-github-data.json --port 4010 --repair

Start hydra-github-bridge:

    cabal run hydra-github-bridge

## Step 5: Send fake requests

To test the bridge, you'll need a sample GitHub webhook payload. You can grab recent
deliveries from the GitHub App settings. Then compact, resign and send it:

    # Compact the payload and remove trailing newlines
    jq --compact-output . sample.payload.txt | tr -d "\n" > sample.payload-compact.txt

    # Generate the signature
    SHA1_SIG=$(openssl dgst -r -sha1 -hmac "$GITHUB_WEBHOOK_SECRET" "sample.payload-compact.txt" | awk '{print $1}')

    # Fake the GitHub request
    curl \
      -H "Request-Method: POST" \
      -H "Accept: */*" \
      -H "Content-Type: application/json" \
      -H "X-GitHub-Event: pull_request" \
      -H "X-GitHub-Hook-Installation-Target-ID: 12345" \
      -H "X-GitHub-Hook-Installation-Target-Type: integration" \
      -H "X-Hub-Signature: sha1=${SHA1_SIG}" \
      -d "@sample.payload-compact.txt" \
      -i http://localhost:8080/hook

This will trigger the bridge to create a project in your local Hydra instance. Once Hydra
finishes evaluation, it will start sending messages to the mock GitHub server, which you
can see in the mockoon-cli console output.
