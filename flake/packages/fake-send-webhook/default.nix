{
  perSystem = {
    config,
    pkgs,
    ...
  }: {
    legacyPackages = pkgs;

    packages.fake-send-webhook = pkgs.writeShellApplication {
      name = "fake-send-webhook";
      runtimeInputs = with pkgs; [jq curl openssl];

      # Usage:
      # WEBHOOK_SECRET=TOPSECRET fake-send-webhook http://hydra-bridge.example.com EVENT < payload.txt
      text = ''
        WEBHOOK_SECRET=''${WEBHOOK_SECRET:-TOPSECRET}
        HOOK_URL=''${1:-http://localhost:8811}
        EVENT=''${2:-pull_request}
        tmp=$(mktemp webhook.XXXX)

        jq --compact-output . \
          | tr -d "\n" \
          > "$tmp"

        SHA1_SIG=$(openssl dgst -r -sha1 -hmac "$WEBHOOK_SECRET" "$tmp" | awk '{print $1}')

        curl \
          -i \
          -H "Accept: */*" \
          -H "Content-Type: application/json" \
          -H "User-Agent: fake-send-webhook" \
          -H "X-GitHub-Event: $EVENT" \
          -H "X-GitHub-Hook-Installation-Target-ID: 98765" \
          -H "X-GitHub-Hook-Installation-Target-Type: integration" \
          -H "X-Hub-Signature: sha1=''${SHA1_SIG}" \
          -d "@$tmp" \
          --fail \
          "$HOOK_URL"

        rm "$tmp"
      '';
    };

    apps.fake-send-webhook.program = config.packages.fake-send-webhook;
  };
}
