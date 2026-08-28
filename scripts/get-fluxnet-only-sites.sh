#!/usr/bin/env bash

SNAPSHOT=$(find data-raw -name fluxnet_shuttle_snapshot_*.csv | sort | tail -n1)

if [[ ! -f $SNAPSHOT ]]; then
  echo "No snapshot file found. Run fluxnet-shuttle listall --output-dir data-raw to generate one."
  exit 1
fi

echo "Using fluxnet-shuttle snapshot file: $SNAPSHOT"

SITES=()
PRESENT=()
while IFS= read -r line; do
  site="$line"
  present=$(find data-raw/FLUXNET -name "*_${site}_FLUXNET_*.zip")
  if [[ -n $present ]]; then
    PRESENT+=($site)
  else
    SITES+=($site)
  fi
done < <(pixi run Rscript - <<'EOF'
sites <- readr::read_csv("data-core/site_info.csv", col_select = c("site_ID", "source"), show_col_types = FALSE)
fluxnet_sites <- sites |>
  dplyr::filter(.data$source == "FLUXNET") |>
  dplyr::arrange(.data$site_ID) |>
  dplyr::pull(.data$site_ID)
cat(fluxnet_sites, sep="\n")
EOF
)

echo "Skipping existing sites: ${PRESENT[*]}"
echo "Downloading data for sites: ${SITES[*]}"

if [[ ! "${#SITES[@]}" -gt 0 ]]; then
  echo "No sites to download. Exiting."
  exit 0
fi

bash workflows/92-download-fluxnet.sh \
  --snapshot "$SNAPSHOT" \
  --sites \
  $SITES

# SITES_CSV=$(IFS=,; echo "${SITES[*]}")

# pixi run Rscript workflows/01_01_estimate_soil_temperature_at_some_sites.R \
#   --sites=$SITES_CSV
#
# pixi run Rscript workflows/01_02a_filter_high_quality_night_respiration_EuroFlux.R \
#   --sites=$SITES_CSV
