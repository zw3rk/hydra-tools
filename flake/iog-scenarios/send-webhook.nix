{pkgs}: pkgs.writeShellApplication {
  name = "iog-send-webhook";
  runtimeInputs = with pkgs; [jq curl openssl];

  # Usage:
  # WEBHOOK_SECRET=TOPSECRET iog-send-webhook http://localhost:8811/hook EVENT < payload.txt
  #
  # Local-batch copy of the upstream fake-send-webhook helper. Signs the
  # payload with the shared webhook secret using openssl (existing lib only;
  # no hand-rolled crypto) and POSTs it to the bridge. All endpoints are
  # loopback/private only.
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
      -H "User-Agent: iog-send-webhook" \
      -H "X-GitHub-Event: $EVENT" \
      -H "X-GitHub-Hook-Installation-Target-ID: 98765" \
      -H "X-GitHub-Hook-Installation-Target-Type: integration" \
      -H "X-Hub-Signature: sha1=''${SHA1_SIG}" \
      -d "@$tmp" \
      --fail \
      "$HOOK_URL"

    rm "$tmp"
  '';
}
