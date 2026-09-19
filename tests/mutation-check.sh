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
add "TS substitution wholesale, not overlaid" \
    R/soil-temp-columns.R \
    '  ts[!is.na(ts_pred)] <- ts_pred[!is.na(ts_pred)]
  ts
}' \
    '  ts_pred
}'
add "TS fit domain loses the TA > 0 restriction" \
    R/soil-temp-columns.R \
    '    ac = ac[ac$TA > 0, ],' \
    '    ac = ac,'
add "TS fit domains collapsed into one" \
    R/soil-temp-columns.R \
    '    night = nightNEE,' \
    '    night = ac[ac$TA > 0, ],'
add "TS bounds taken over the whole record" \
    R/soil-temp-columns.R \
    '  gs <- ts[dplyr::between(doy, gStart, gEnd)]' \
    '  gs <- ts'
add "TS bounds taken from the nighttime table" \
    R/soil-temp-columns.R \
    '  bounds <- ts_bounds(ac$TS, ac$DOY, gStart, gEnd)' \
    '  bounds <- ts_bounds(nightNEE$TS, nightNEE$DOY, gStart, gEnd)'
add "TS selection ignored, always measured" \
    R/soil-temp-columns.R \
    '  dat[["TS"]] <- dat[[ts_col]]' \
    '  dat[["TS"]] <- dat[["TS_measured"]]'
add "TS bounds fall back to the first row" \
    R/soil-temp-columns.R \
    '  row <- ts_bounds[ts_bounds[["ts_col"]] == ts_col, ]' \
    '  row <- ts_bounds[1, ]'
add "reanalysis SWC preferred over measured" \
    R/soil-water-columns.R \
    '  if (isTRUE(site_info[["SWC_use"]])) return("SWC_measured")
  if (isTRUE(direct)) return("SWC_era5")
  NA_character_' \
    '  if (isTRUE(direct)) return("SWC_era5")
  if (isTRUE(site_info[["SWC_use"]])) return("SWC_measured")
  NA_character_'
add "SWC fallback applied to the total model too" \
    R/soil-water-columns.R \
    '  if (isTRUE(direct)) return("SWC_era5")
  NA_character_' \
    '  "SWC_era5"'
add "SWC selection ignored, always measured" \
    R/soil-water-columns.R \
    '  dat[["SWC"]] <- dat[[swc_col]]' \
    '  dat[["SWC"]] <- dat[["SWC_measured"]]'
add "FI-Sod regressions fitted on swapped windows" \
    R/prepare-site-data.R \
    '  to_deep <- lm(TS_F_MDS_2 ~ TS_F_MDS_1, data = a[early, ], na.action = na.omit)
  to_shallow <- lm(TS_F_MDS_1 ~ TS_F_MDS_2, data = a[late, ], na.action = na.omit)' \
    '  to_deep <- lm(TS_F_MDS_2 ~ TS_F_MDS_1, data = a[late, ], na.action = na.omit)
  to_shallow <- lm(TS_F_MDS_1 ~ TS_F_MDS_2, data = a[early, ], na.action = na.omit)'
add "FI-Sod rebuild applied to the good period" \
    R/prepare-site-data.R \
    '  bad <- a$YEAR <= FI_SOD_TS_BAD_THROUGH' \
    '  bad <- a$YEAR > FI_SOD_TS_BAD_THROUGH'
add "FI-Sod skip guard removed" \
    R/prepare-site-data.R \
    '  if (n_early == 0 || n_late == 0 || !any(bad)) {' \
    '  if (FALSE) {'
add "FI-Sod windows widened to year boundaries" \
    R/prepare-site-data.R \
    'FI_SOD_EARLY_WINDOW <- c("200101010000", "200205232300")
FI_SOD_LATE_WINDOW <- c("200602182330", "201412230330")' \
    'FI_SOD_EARLY_WINDOW <- c("200101010000", "200512312330")
FI_SOD_LATE_WINDOW <- c("200601010000", "201412312330")'
add "RH-to-VPD conversion flagged for every site" \
    R/ameriflux.R \
    'convert_rh <- !is.na(site_info$RH)' \
    'convert_rh <- TRUE'
add "RH-to-VPD decision dropped from the result" \
    R/ameriflux.R \
    'list(ac = ac, convert_rh = convert_rh)' \
    'list(ac = ac)'

add "dev sample drops the random-forest site" \
    R/constants.R \
    'DEV_SITES <- c("DE-RuC", "DE-Hte", "DE-Akm", "FI-Sod", "SE-Deg", "NL-Loo")' \
    'DEV_SITES <- c("DE-RuC", "DE-Hte", "FI-Sod", "SE-Deg", "NL-Loo")'
add "dev sample drops the multi-product splices" \
    R/constants.R \
    'DEV_SITES <- c("DE-RuC", "DE-Hte", "DE-Akm", "FI-Sod", "SE-Deg", "NL-Loo")' \
    'DEV_SITES <- c("DE-RuC", "DE-Hte", "DE-Akm")'
add "unknown THERMAL_SITES falls back silently" \
    R/utils.R \
    'stop("THERMAL_SITES must be \"dev\" or \"all\", not ", shQuote(scope), ".")' \
    'return(handled)'

add "structure-only run reports a fitted outcome" \
    R/total_tas.R \
    '    return(list(
      outcome = NULL,
      outcome_siteyear = window_results_df,' \
    '    return(list(
      outcome = window_results_df,
      outcome_siteyear = window_results_df,'
add "structure-only years left without a status" \
    R/total_tas.R \
    'year_result@status <- "not_fitted"' \
    'year_result@status <- NA_character_'
add "skipped years left without a reason" \
    R/total_tas.R \
    'year_result@status <- if (nrow(data_subset) <= 25) {' \
    'year_result@status <- if (FALSE) {'

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
