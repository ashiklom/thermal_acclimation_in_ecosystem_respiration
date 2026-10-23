#!/usr/bin/env bash
# Pack the analysis website (pages and the data files they read) into a
# tarball that runs with nothing but python3's http.server.
#
# Usage (from the repository root): scripts/website-bundle.sh [BUNDLE_DIR]
#   BUNDLE_DIR  directory the site is copied to (default
#               website-bundle/thermal-acclimation-website); the tarball is
#               written next to it as BUNDLE_DIR.tar.gz.
#
# Run with `pixi run website-bundle [BUNDLE_DIR]`, which rebuilds the data
# files (website-build) first.

set -euo pipefail

bundle=${1:-website-bundle/thermal-acclimation-website}
bundle=${bundle%/}
marker=.website-bundle

for f in analysis-website/data/sites.json analysis-website/vendor/plotly.min.js; do
  if [[ ! -f $f ]]; then
    echo "error: $f is missing (run from the repository root, after website-build)" >&2
    exit 1
  fi
done

# Replace only a directory this script made before.
if [[ -e $bundle ]]; then
  if [[ ! -f $bundle/$marker ]]; then
    echo "error: $bundle exists and is not a previous website bundle; not overwriting it" >&2
    exit 1
  fi
  rm -rf "$bundle"
fi
mkdir -p "$bundle"
touch "$bundle/$marker"

# build.py needs the repository's results; the Caddyfile is replaced by http.server.
rsync -a --exclude build.py --exclude Caddyfile analysis-website/ "$bundle/"

name=$(basename "$bundle")
cat > "$bundle/README.md" <<EOF
# Thermal acclimation analysis website

A static website: the pages read their data from \`data/\`, so they must be served
over HTTP (opening \`index.html\` from disk does not work, as browsers block
\`fetch\` on \`file://\`). Python 3's standard library is all that is needed.

1. Unpack: \`tar -xzf $name.tar.gz\`
2. Serve: \`python3 -m http.server 8770 --bind 127.0.0.1 --directory $name\`
   (on Windows: \`py -m http.server 8770 --bind 127.0.0.1 --directory $name\`)
3. Open <http://127.0.0.1:8770> in a browser; stop the server with Ctrl-C.
EOF

tarball="$bundle.tar.gz"
tar -czf "$tarball" --exclude "$marker" -C "$(dirname "$bundle")" "$name"
echo "Wrote $tarball ($(du -h "$tarball" | awk '{print $1}'))"
