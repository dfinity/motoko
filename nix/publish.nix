# Build a script to copy the motoko release to the
# dfinity-download S3 bucket which is the origin of the
# download.dfinity.systems world-wide CDN.
#
# This script will be executed by DFINITY's Continuous Deployment
# system. That system will also set the correct AWS credentials and the
# DFINITY_DOWNLOAD_BUCKET environment variable.
#
# TODO: We want to include more than just `moc` and `mo-ide` but for now they
# should show that we can build a tarball from multiple binaries
{ pkgs, releaseVersion, derivations }:
let
  s3cp = pkgs.lib.writeCheckedShellScriptBin "s3cp" [] ''
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

  slack = pkgs.lib.writeCheckedShellScriptBin "slack" [] ''
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

  v = releaseVersion;

  mkMotokoTarball = derivations:
    pkgs.runCommandNoCC "motoko-${v}.tar.gz" {
      allowedRequisites = [];
    } (
      ''
      tmp=$(mktemp -d)
      ''
    + builtins.concatStringsSep "" (builtins.map (d: ''
      cp -v ${d}/bin/* $tmp
      '') derivations)
    + ''
      chmod 0755 $tmp/*
      tar -czf "$out" -C $tmp/ .
      ''
    );


in
{
  motokoTarBall = mkMotokoTarball derivations;
  motoko = pkgs.lib.linuxOnly (
    pkgs.lib.writeCheckedShellScriptBin "activate" [] ''
      set -eu
      PATH="${pkgs.lib.makeBinPath [ s3cp slack ]}"

      v="${v}"
      cache_long="max-age=31536000" # 1 year

      file="motoko-$v.tar.gz"
      dir="motoko/$v"

      s3cp "${mkMotokoTarball (builtins.map (d: d.x86_64-linux) derivations)}" "$dir/x86_64-linux/$file" "application/gzip" "$cache_long"
      s3cp "${mkMotokoTarball (builtins.map (d: d.x86_64-darwin) derivations)}" "$dir/x86_64-darwin/$file" "application/gzip" "$cache_long"

      slack "$SLACK_CHANNEL_BUILD_NOTIFICATIONS_WEBHOOK" <<EOI
      *motoko-$v* has been published to DFINITY's CDN :champagne:!
      - https://$DFINITY_DOWNLOAD_DOMAIN/$dir/x86_64-linux/$file
      - https://$DFINITY_DOWNLOAD_DOMAIN/$dir/x86_64-darwin/$file
      EOI
    ''
  );
}
