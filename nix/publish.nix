# Build a script to copy the motoko release to the
# dfinity-download S3 bucket which is the origin of the
# download.dfinity.systems world-wide CDN.
#
# This script will be executed by DFINITY's Continuous Deployment
# system. That system will also set the correct AWS credentials and the
# DFINITY_DOWNLOAD_BUCKET environment variable.
{ pkgs, releaseVersion, derivations }:
let
  s3cp = pkgs.writeShellScriptBin "s3cp" ''
    set -eu
    PATH="${pkgs.lib.makeBinPath [ pkgs.awscli ]}"
    src="$1"; dst="$2"; contentType="$3"; cacheControl="$4"
    dstUrl="s3://$DFINITY_DOWNLOAD_BUCKET/$dst"
    if [ -d "$src" ]; then
      echo "Can't copy $src to $dstUrl because it's a directory. Please specify a file instead." 1>&2; exit 1;
    fi
    echo "Uploading $src to $dstUrl (--cache-control $cacheControl, --content-type $contentType)..."
    aws s3 cp "$src" "$dstUrl" \
      --cache-control "$cacheControl" \
      --no-guess-mime-type --content-type "$contentType" \
      --no-progress
  '';

  slack = pkgs.writeShellScriptBin "slack" ''
    set -eu
    PATH="${pkgs.lib.makeBinPath [ pkgs.jq pkgs.curl ]}"
    slack_channel_webhook="$1"
    msg="$(</dev/stdin)"
    echo {} | jq --arg msg "$msg" '.blocks=[
      {
        "type" : "section",
        "text" : {
          "type" : "mrkdwn",
          "text" : $msg
        }
      }
    ]' | curl -X POST --data @- "$slack_channel_webhook" \
           --header "Content-Type: application/json" --silent --show-error
  '';
in
rec {
  motoko =
    pkgs.writeShellScriptBin "activate" (''
      set -eu
      PATH="${pkgs.lib.makeBinPath [ s3cp slack ]}"

      v="${releaseVersion}"
      cache_long="max-age=31536000" # 1 year

      file="motoko-$v.tar.gz"
      dir="motoko/$v"
      '' +
      pkgs.lib.concatMapStrings (d: ''
        s3cp "${d}" "$dir/${d.meta.path}" "${d.meta.content-type}" "$cache_long"
     '') derivations + ''

      slack "$SLACK_CHANNEL_BUILD_NOTIFICATIONS_WEBHOOK" <<EOI
      *motoko-$v* has been published to DFINITY's CDN :champagne:!
      '' +
      pkgs.lib.concatMapStrings (d: ''
      - https://$DFINITY_DOWNLOAD_DOMAIN/$dir/${d.meta.path}
      '') derivations + ''
      EOI
    '');
}
