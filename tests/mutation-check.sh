#!/usr/bin/env bash
#
# Does the unit suite actually catch the regressions it was written for?
#
# A suite that passes on correct code proves nothing on its own. This
# reintroduces each fixed bug, one at a time, and checks that
# `tests/run-tests.R` goes red. A mutation that leaves the suite green is a
# coverage hole, and is reported as one.
#
# Edits files under R/ and restores them from git, so it refuses to run unless
# that directory is clean.
set -uo pipefail
cd "$(dirname "$0")/.."

if ! git diff --quiet -- R/ || ! git diff --cached --quiet -- R/; then
  echo "R/ has uncommitted changes; commit or stash them first." >&2
  exit 2
fi

RSCRIPT=${RSCRIPT:-.pixi/envs/default/bin/Rscript}
trap 'git checkout -- R/' EXIT

NAMES=(); FILES=(); FROM=(); TO=()
add() { NAMES+=("$1"); FILES+=("$2"); FROM+=("$3"); TO+=("$4"); }

add "day/night timezone (blocker 2)" \
    R/ameriflux.R \
    'tz = tz_site' \
    'tz = "UTC"'
add "FC mask via na_if (high 1)" \
    R/ameriflux.R \
    'ac$NEE[is.na(a[[site_info[["FC"]]]])] <- NA_real_' \
    'ac <- dplyr::mutate(ac, NEE = dplyr::na_if(.data$NEE, is.na(a[[site_info[["FC"]]]])))'
add "SWC_use presence test (high 2)" \
    R/ameriflux.R \
    'if (isTRUE(site_info$SWC_use))' \
    'if (!is.na(site_info$SWC_use))'
add "AmeriFlux cut-off capped (high 3)" \
    R/respiration_helpers.R \
    'uncapped = nee_min * 0.2,' \
    'uncapped = max(nee_min * 0.2, -0.8),'
add "FI-Sod/DE-RuC cut-off (high 3)" \
    R/respiration_helpers.R \
    'zero = 0.0' \
    'zero = max(nee_min * 0.2, -0.8)'
add "ERA5 percent conversion (blocker 1)" \
    R/total_tas.R \
    'SWC = .data$SWC * ERA5_SWC_TO_PERCENT' \
    'SWC = .data$SWC'
add "splice keeps later product on overlap" \
    R/prepare-site-data.R \
    'combined <- dplyr::bind_rows(combined, nxt[nxt$TIMESTAMP_START > tail_start, ])' \
    'combined <- dplyr::bind_rows(combined[combined$TIMESTAMP_START < nxt$TIMESTAMP_START[1], ], nxt)'
add "splice trusts declared order" \
    R/prepare-site-data.R \
    'parts <- parts[order(vapply(parts, function(d) d$TIMESTAMP_START[1], ""))]' \
    'parts <- parts'
add "single-product source list" \
    R/utils.R \
    'sources <- trimws(unlist(strsplit(site_info[["source"]], "+", fixed = TRUE)))' \
    'sources <- site_info[["source"]]'
add "flux timestamps typed numeric" \
    R/constants.R \
    'TIMESTAMP_START = readr::col_character(),' \
    'TIMESTAMP_START = readr::col_double(),'
add "flux columns typed character" \
    R/constants.R \
    '.default = readr::col_double()' \
    '.default = readr::col_character()'
add "AmeriFlux table left a base data.frame" \
    R/ameriflux.R \
    '  ) |>
    tibble::as_tibble()
  a[a == -9999] <- NA' \
    '  )
  a[a == -9999] <- NA'

caught=0; holes=0
for i in "${!NAMES[@]}"; do
  if ! python3 - "${FILES[$i]}" "${FROM[$i]}" "${TO[$i]}" <<'PY'
import io, sys
path, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
s = io.open(path, encoding="utf-8").read()
if s.count(old) != 1:
    sys.exit(f"{path}: {s.count(old)} matches for {old!r}")
io.open(path, "w", encoding="utf-8").write(s.replace(old, new))
PY
  then
    echo "  SKIP    ${NAMES[$i]} (mutation no longer applies -- update this script)"
    git checkout -- R/
    continue
  fi

  if "$RSCRIPT" tests/run-tests.R >/dev/null 2>&1; then
    echo "  HOLE    ${NAMES[$i]} -- suite still passed"
    holes=$((holes + 1))
  else
    echo "  caught  ${NAMES[$i]}"
    caught=$((caught + 1))
  fi
  git checkout -- R/
done

echo
echo "  $caught caught, $holes uncaught"
[ "$holes" -eq 0 ]
