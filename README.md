# Hydra Tools

**Components and replacements to make Hydra suck less**

A collection of bridge tools that enhance [Hydra](https://github.com/NixOS/hydra) (the Nix-based continuous integration system) with modern CI/CD workflow integrations.

## What is this project?

Hydra Tools provides three essential bridge components that address key limitations in Hydra's integration with modern development workflows:

- **🔄 hydra-github-bridge**: Webhook/Status bridge between GitHub ↔ Hydra
- **📦 hydra-attic-bridge**: Artifact bridge from Hydra → Attic binary cache

## Problems Solved

### The Hydra GitHub Integration Problem

Hydra is fundamentally designed as a poll-driven CI system, which creates several issues when integrating with GitHub:

1. **Rate Limiting**: Hydra's GitHub plugin uses declarative jobsets created on a polling interval, leading to excessive GitHub API requests that quickly hit rate limits
2. **Delayed Feedback**: Poll-based checks mean slower response times for PR status updates
3. **Resource Waste**: Constant polling consumes unnecessary resources
4. **Poor Developer Experience**: Developers don't get timely feedback on their PRs

### Event-Driven Solution

Instead of modifying Hydra's Perl codebase, these tools provide clean Haskell-based bridges that:

- Enable **event-driven CI** through GitHub webhooks
- Provide **real-time status updates** back to GitHub
- **Reduce API rate limiting** by eliminating unnecessary polling
- **Improve binary cache utilization** through automatic artifact uploads

## What Each Component Offers

### 🔄 hydra-github-bridge

**GitHub ↔ Hydra Webhook Bridge**

Receives GitHub webhook events and translates them into Hydra API calls:
Monitors Hydra build events and reports status back to GitHub:

- **GitHub Events**: Automatically creates/updates/removes Hydra jobsets for PRs
- **Real-time Status Updates**: GitHub Check Runs updated as builds progress
- **Event-Driven**: Eliminates the need for constant polling
- **Rate Limit Friendly**: Only makes API calls when necessary

**Key Features:**
- Handles PR lifecycle (opened, synchronized, closed)
- Handles real-time Hydra updates via PostgreSQL LISTEN/NOTIFY
- GitHub App integration with JWT authentication
- Comprehensive build log inclusion in GitHub checks

**Usage:** See [Github Integration](docs/github-integration.md)

- **Real-time Status Updates**: GitHub Check Runs updated as builds progress
- **Detailed Build Information**: Links to Hydra build pages, logs, and artifacts
- **Build Result Reporting**: Success/failure status with detailed error information
- **Dependency Tracking**: Reports status for dependent builds

**Key Features:**
- PostgreSQL LISTEN/NOTIFY for real-time updates
- GitHub App integration with JWT authentication
- Comprehensive build log inclusion in GitHub checks
- Support for compressed build logs (bz2)

### 📦 hydra-attic-bridge

**Hydra → Attic Binary Cache Bridge**

Automatically uploads successful build artifacts to Attic binary cache:

- **Automatic Upload**: Successful builds are queued for upload to Attic
- **Retry Logic**: Failed uploads are retried with exponential backoff
- **Database Triggers**: Uses PostgreSQL triggers for reliable event capture
- **Efficient Processing**: Only uploads artifacts that haven't been uploaded

**Key Features:**
- PostgreSQL trigger-based event capture
- Configurable retry attempts and delays
- Support for both build steps and build outputs
- Environment-based configuration

## Building and Usage

### Prerequisites

- GHC 9.2+ and Cabal
- PostgreSQL database (for Hydra integration)
- Access to a Hydra instance
- GitHub App credentials (for GitHub integration)
- Attic instance (for binary cache integration)

### Building

```bash
# Build all components
make all

# Build individual components
make hydra-github-bridge  
make hydra-attic-bridge

# Clean build artifacts
make clean
```

### Using Nix

This project provides a complete Nix flake with packages and NixOS modules:

```bash
# Build with Nix
nix build .#hydra-github-bridge
nix build .#hydra-attic-bridge

# Run directly
nix run .#hydra-github-bridge
```

### NixOS Integration

NixOS modules are provided for easy deployment:

```nix
{
  imports = [ inputs.hydra-tools.nixosModules.github-hydra-bridge ];
  
  services.github-hydra-bridge = {
    enable = true;
    hydraHost = "hydra.example.com";
    port = 8811;
    # ... additional configuration
  };
}
```

## Configuration

### Environment Variables

#### hydra-github-bridge
- `HYDRA_HOST`: Hydra server hostname
- `HYDRA_DB`: Hydra database hostname
- `HYDRA_DB_USER`: Hydra database username
- `HYDRA_DB_PASS`: Hydra database password
- `HYDRA_USER`: Hydra username
- `HYDRA_PASS`: Hydra password
- `HYDRA_STATE_DIR`: Directory for state files
- `GITHUB_USER_AGENT`: User agent for API requests
- `GITHUB_APP_ID`: GitHub App ID
- `GITHUB_APP_INSTALL_IDS`: GitHub App installation IDs
- `GITHUB_WEBHOOK_SECRET`: The secret for GitHub webhook payloads
- `GITHUB_APP_KEY_FILE`: GitHub App private key file
- `GITHUB_ENDPOINT_URL`: The GitHub API base URL (useful for testing)
- `PORT`: Server port (default: 8080)

#### hydra-attic-bridge  
- `ATTIC_HOST`: Attic server URL
- `ATTIC_CACHE`: Cache name
- `ATTIC_TOKEN`: Authentication token
- `HYDRA_DB*`: Database connection settings

### Database Setup

For `hydra-attic-bridge`, run the SQL setup:

```bash
psql -d hydra -f hydra-attic-bridge/setup.sql
```

## Motivation

The primary motivation for this project is to bridge the gap between Hydra's design philosophy and modern development workflows. While Hydra is an excellent CI system for Nix projects, its polling-based approach doesn't align well with the event-driven nature of modern Git hosting platforms.

By providing these bridge components, we enable:

1. **Better Developer Experience**: Immediate feedback on PRs through GitHub checks
2. **Resource Efficiency**: Event-driven processing reduces unnecessary computation
3. **API Rate Limit Compliance**: Eliminates excessive GitHub API calls
4. **Improved Caching**: Automatic binary cache population improves build times
5. **Production Ready**: Battle-tested components used in production environments

## Contributing

This project is written in Haskell and uses Cabal for package management. The codebase is structured as multiple packages within a single repository for easy maintenance and shared dependencies.

## License

This project is licensed under the BSD-3-Clause license. See the LICENSE files in individual component directories for details.
