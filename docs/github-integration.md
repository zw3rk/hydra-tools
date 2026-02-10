# Integrating with GitHub

GitHub Hydra Bridge integrates Hydra CI with GitHub by monitoring for repository events and
reporting Hydra build statuses back to GitHub.

## Prerequisites

Before installation, ensure you have:

 * A public Hydra instance accessible on HTTPS
 * Administrator access to a GitHub organization
 * A NixOS system to install Hydra Tools

We recommend installing HydraTool on the same server as your Hydra server, as it requires
direct access to the Hydra database.

## Step 1: Register a New GitHub App

Navigate to the _Register a New GitHub App_ page:

 1. In the upper-right corner of any page on GitHub, click your profile picture, and 
    navigate to your account settings.
 3. Click Organizations.
 4. To the right of the organization, click Settings.
 5. In the left sidebar, click Developer settings, then GitHub Apps.
 6. Click New GitHub App.

Use the following settings:

 * GitHub App Name: Enter a descriptive, short name for the app.
 * Homepage URL: Enter a URL for the App. If you like, you can use 
   https://github.com/input-output-hk/hydra-tools.
 * WebHook 
   * Active: Make sure this is selected.
   * Webhook URL: Enter the target URL for your GitHub Hydra Bridge. For example, 
     https://hydra-tools.example.com/hook.
   * Secret: Enter a secret token to secure the webhook. GitHub highly recommends that 
     you set a webhook secret.
 * SSL verification: Select whether to enable SSL verification. GitHub highly recommends
   that you enable SSL verification. It is highly recommended to leave this enabled.
 * Permissions: Select following permissions
   * Checks: Read and Write
   * Commit Statuses: Read and Write
   * Contents: Read-only
   * Metadata: Read-only (automatically set)
 * Subscribe to Events: Select the following events
   * Check run
   * Check suite
   * Pull request
   * Push
 * Where can this GitHub App be installed?: Select _Any account_ to allow installation
   across multiple organizations, or _Only this account_ to restrict it to your current
   organization.

Click _Create GitHub App_. This will take you to the _GitHub App settings_ page. Take note
of the _App ID_, you'll need it to configure the GitHub Hydra Bridge server.

## Step 2: Generate a Private Key

The server needs a private key to authenticate as a GitHub App:

 1. On the _GitHub App settings_ page, scroll down to the _Private Keys_ section.
 2. Click _Generate Private Key_
 3. Download the private key, you'll need this later.

## Step 3: Install the GitHub App

After registering the GitHub App, install it to give it access to your repositories:

 1. On the _GitHub App Settings_ page, click _Install App_ on the left sidebar.
 2. Next to your organization, click _Install_.

After installation, note the _Installation ID_ (it's part of the URL), you'll need it to
configure the GitHub Hydra Bridge server.

## Step 4: Add Repository Webhooks

In addition to app-level webhooks, you also need to configure repository-specific webhooks.

 1. Navigate the the repository on GitHub, then click on Settings.
 2. On the left sidebar, click Webhooks, then Add Webhook.
 3. Enter the following settings:
    * Payload URL: Enter the target URL for your GitHub Hydra Bridge. For example, 
      https://hydra-tools.example.com/hook.
    * Content-type: Select _application/json_.
    * Enable SSL verification: It is highly recommended to leave this enabled
    * Which events would you like to trigger this webhook?: Select _Let me select individual
      events_, and select:
      * Pull requests
      * Pushes
    * Active: Make sure this is select.

Click _Add webhook_

## Step 5: Run Database Migrations

Hydra Tools requires extra database tables. Run the following SQL script on the Hydra
database:

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

## Step 6: Install GitHub Bridge
Import the NixOS modules:

    inputs = {
      hydraTools.url = github:input-output-hk/hydra-tools;
    };

    outputs = inputs: {
        nixosConfigurations.my-host = nixpkgs.lib.nixosSystem {
          modules = [
            inputs.hydraTools.nixosModules.hydra-github-bridge
          ];
        };
    };

    nixConfig = {
      extra-substituters = [
        "https://cache.iog.io"
      ];
      extra-trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
    };

Configure the GitHub bridges:

    services = {
      hydra-github-bridge.public = {
        enable = true;
        ghAppId = 123456789; # Use the App ID from Step 1
        ghAppInstallIds = "[(\"ORG_NAME\", 987654321)]"; # Use your Installation ID from Step 3
        ghAppKeyFile = "/path/to/secrets/gh-app-key";    # Path to the private key file from Step 2
        ghSecretFile = "/path/to/secrets/hydra-gh-secret"; # Use the Webhook Secret from Step 1
        hydraHost = "hydra.example.com"; # Use the public Hydra URL
        hydraUser = "bridge";
        hydraPassFile = "/path/to/secrets/hydra-tools-pass";
        port = 8811;
      };
    };

After deploying, the GitHub Hydra Bridge server will run on port 8811. We recommend
recommended to using a reverse proxy to enable HTTPS.

## Next Steps

At this point Hydra Tools should begin receiving webhooks from GitHub and delivering build
statuses. You can verify connectivity by redelivering the initial ping request from
GitHub:

 1. On the _GitHub App settings_ page, click _Advanced_ on the left sidebar.
 2. Click the ellipsis next to the `ping` request
 3. Click redeliver
