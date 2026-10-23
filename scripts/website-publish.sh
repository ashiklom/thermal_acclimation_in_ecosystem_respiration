#!/usr/bin/env bash
# Deploy the analysis website bundle to GitHub Pages: upload the tarball made
# by website-bundle.sh as the asset of the `website` release, then run the
# Pages workflow (.github/workflows/pages.yml, on main), which deploys it.
# The site's data stay out of git, so clones of the repository do not grow.
#
# Usage (from the repository root): scripts/website-publish.sh
#   The repository is origin's; set GH_REPO=OWNER/REPO to publish elsewhere.
#
# Run with `pixi run website-publish`, which rebuilds the data files and the
# bundle (website-build, website-bundle) first. Needs an authenticated `gh`.

set -euo pipefail

tarball=website-bundle/thermal-acclimation-website.tar.gz
release=website

if [[ ! -f $tarball ]]; then
  echo "error: $tarball is missing (run from the repository root, after website-bundle)" >&2
  exit 1
fi
if ! gh auth status >/dev/null 2>&1; then
  echo "error: gh is not authenticated (run \`gh auth login\`)" >&2
  exit 1
fi

if [[ -z ${GH_REPO:-} ]]; then
  GH_REPO=$(git remote get-url origin | sed -E 's#^(https://github\.com/|git@github\.com:)##; s#\.git$##')
fi
export GH_REPO

# Where the site was built from, recorded in the release notes.
sha=$(git rev-parse --short HEAD)
if [[ -n $(git status --porcelain) ]]; then
  sha="$sha-dirty"
fi
branch=$(git rev-parse --abbrev-ref HEAD)
built=$(date -u +%Y-%m-%dT%H:%MZ)

if ! gh release view "$release" >/dev/null 2>&1; then
  gh release create "$release" --prerelease --target main \
    --title "Analysis website (latest build)" --notes "Created by scripts/website-publish.sh"
fi
# gh can retry an upload that already went through and then fail with
# "ReleaseAsset.name already exists"; the asset is good if its digest matches.
if ! gh release upload "$release" "$tarball" --clobber; then
  local_digest=sha256:$(shasum -a 256 "$tarball" | awk '{print $1}')
  remote_digest=$(gh api "repos/$GH_REPO/releases/tags/$release" \
    --jq ".assets[] | select(.name == \"$(basename "$tarball")\") | .digest")
  if [[ $remote_digest != "$local_digest" ]]; then
    echo "error: uploading $tarball failed" >&2
    exit 1
  fi
  echo "The uploaded asset matches $tarball; continuing."
fi
gh release edit "$release" --notes "Built from $branch@$sha at $built by scripts/website-publish.sh."

gh workflow run pages.yml --ref main
owner=${GH_REPO%%/*}
echo "Uploaded $tarball to $GH_REPO release '$release' and started the Pages workflow."
echo "Follow it with \`gh run watch -R $GH_REPO\`; the site is at https://$owner.github.io/${GH_REPO#*/}/"
