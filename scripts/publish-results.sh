#!/usr/bin/env bash
# Publish saved result snapshots (results/<id>/, scripts/save-results.R) as
# GitHub releases, one release per commit the snapshots were made at: the
# release of the stage tag (v2-*) pointing at the manifest's git_sha, or of a
# new tag results-<id> there. Each snapshot is one asset, <id>.tar.xz (the
# CSVs as they are: smaller than parquet after xz, and byte-lossless).
# Snapshots already uploaded are skipped.
#
# Usage (from the repository root):
#   bash scripts/publish-results.sh [ID ...]     (default: every snapshot)
# Environment: RESULTS_REPO (default ashiklom/thermal_acclimation_in_ecosystem_respiration),
#   REFACTOR_LOG (default ../refactor-v2-log.md; its stage section goes in the notes)

set -euo pipefail
repo="${RESULTS_REPO:-ashiklom/thermal_acclimation_in_ecosystem_respiration}"
log="${REFACTOR_LOG:-../refactor-v2-log.md}"
if [ $# -gt 0 ]; then ids=("$@"); else ids=($(ls results/*/manifest.json | xargs -n1 dirname | xargs -n1 basename)); fi
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

field() { python3 -c 'import json,sys; v=json.load(open(sys.argv[1])).get(sys.argv[2], ""); print(",".join(v) if isinstance(v, list) else v)' "results/$1/manifest.json" "$2"; }

for id in "${ids[@]}"; do
  test -f "results/$id/manifest.json" || { echo "$id: no results/$id/manifest.json" >&2; exit 1; }
  sha="$(git rev-parse "$(field "$id" git_sha)^{commit}")"
  tag="$(git tag --points-at "$sha" -l 'v2-*' | head -1)"
  if [ -z "$tag" ]; then
    tag="results-$id"
    if ! git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
      git tag -a "$tag" "$sha" -m "Results snapshot $id (commit of its run; no stage tag here)"
    fi
  fi
  git push -q origin "refs/tags/$tag"
  asset="$work/$id.tar.xz"
  tar -C results -cf - "$id" | xz -9e -T0 >| "$asset"

  if gh release view "$tag" --repo "$repo" >/dev/null 2>&1; then
    if gh release view "$tag" --repo "$repo" --json assets -q '.assets[].name' | grep -qx "$id.tar.xz"; then
      echo "$id: already in release $tag"
      continue
    fi
    gh release upload "$tag" "$asset" --repo "$repo"
    # add the snapshot to the notes
    notes="$work/notes.md"
    gh release view "$tag" --repo "$repo" --json body -q .body >| "$notes"
  else
    notes="$work/notes.md"
    : >| "$notes"
  fi
  {
    echo
    echo "### Snapshot \`$id\` ($id.tar.xz)"
    echo
    echo "- stage: $(field "$id" stage); fit profile: $(field "$id" fit_profile); sites: $(field "$id" n_sites); data: $(field "$id" data)"
    echo "- run at commit $(field "$id" git_sha) on $(field "$id" machine), saved $(field "$id" created)"
    echo "- $(field "$id" description)"
    stage="$(field "$id" stage)"
    if [ -f "$log" ] && [ -n "$stage" ]; then
      section="$(awk -v s="$stage" 'index($0, "## " s " ") == 1 || index($0, "## " s " —") == 1 { on = 1; print; next } on && /^## / { exit } on' "$log" | head -c 6000)"
      if [ -n "$section" ] && ! grep -qF "$(echo "$section" | head -1)" "$notes"; then
        echo
        echo "From refactor-v2-log.md:"
        echo
        echo "$section"
      fi
    fi
  } >> "$notes"
  if gh release view "$tag" --repo "$repo" >/dev/null 2>&1; then
    gh release edit "$tag" --repo "$repo" --notes-file "$notes"
  else
    gh release create "$tag" "$asset" --repo "$repo" --verify-tag --latest=false \
      --title "Results: $tag" --notes-file "$notes"
  fi
  echo "$id: release $tag ($(du -h "$asset" | cut -f1))"
done
