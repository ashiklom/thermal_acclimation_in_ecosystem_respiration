#!/bin/bash

# Purpose: Download FLUXNET data using fluxnet-shuttle CLI
# Usage:
#   ./workflows/92-download-fluxnet.sh
#   ./workflows/92-download-fluxnet.sh --sites FR-Pue CZ-RAJ
#   ./workflows/92-download-fluxnet.sh --overwrite

set -euo pipefail

# Defaults
SNAPSHOT_FILE=""
OUTPUT_DIR="data-raw/FLUXNET"
OVERWRITE=false
SITES=()

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -s|--sites)
            shift
            while [[ $# -gt 0 && ! "$1" =~ ^- ]]; do
                SITES+=("$1")
                shift
            done
            ;;
        -f|--snapshot)
            SNAPSHOT_FILE="$2"
            shift 2
            ;;
        -o|--overwrite)
            OVERWRITE=true
            shift
            ;;
        -d|--output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [--sites SITE1 SITE2 ...]"
            echo ""
            echo "Download FLUXNET data using fluxnet-shuttle CLI"
            echo ""
            echo "Options:"
            echo "  -s, --sites SITE1 SITE2  Space-separated list of site IDs to download"
            echo "  -f, --snapshot FILE      Path to snapshot CSV file (default: fluxnet_shuttle_snapshot_*.csv)"
            echo "  -o, --overwrite          Overwrite existing files"
            echo "  -d, --output-dir DIR     Output directory (default: data-raw/FLUXNET)"
            echo "  -h, --help               Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Resolve the snapshot file.
if [[ -n "$SNAPSHOT_FILE" ]]; then
  if [[ ! -f "$SNAPSHOT_FILE" ]]; then
    echo "Error: Snapshot file not found: $SNAPSHOT_FILE" >&2
    exit 1
  fi
else
  SNAPSHOT_FILE=$(find data-raw -maxdepth 1 -name 'fluxnet_shuttle_snapshot_*.csv' | sort | tail -n1)
  if [[ -z "$SNAPSHOT_FILE" ]]; then
    echo "Warning: No fluxnet-shuttle snapshot found in data-raw." >&2
    echo "Downloading one with 'fluxnet-shuttle listall'..." >&2
    pixi run fluxnet-shuttle listall -o data-raw
    SNAPSHOT_FILE=$(find data-raw -maxdepth 1 -name 'fluxnet_shuttle_snapshot_*.csv' | sort | tail -n1)
    if [[ -z "$SNAPSHOT_FILE" ]]; then
      echo "Error: Failed to create snapshot file with 'fluxnet-shuttle listall'." >&2
      exit 1
    fi
  fi
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Build command
CMD=(pixi run fluxnet-shuttle download --snapshot-file "$SNAPSHOT_FILE" --output-dir "$OUTPUT_DIR" --quiet)

# If sites are not specified, download all missing FLUXNET sites from site_info.csv
if [[ ! ${#SITES[@]} -gt 0 ]]; then
  PRESENT=()
  while IFS= read -r line; do
    site="$line"
    present=$(find data-raw/FLUXNET -name "*_${site}_FLUXNET_*.zip")
    if [[ -n "$present" ]] && [[ "$OVERWRITE" != "true" ]]; then
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
  echo "Sites to download: ${SITES[*]}"
fi

if [[ ! "${#SITES[@]}" -gt 0 ]]; then
  echo "No sites to download. Exiting."
  exit 0
fi

CMD+=(--sites "${SITES[@]}")

# Run download
echo "Downloading FLUXNET data..."
"${CMD[@]}"

# Unzip each archive into its site-specific FLUXNET directory.
echo "Extracting FLUXNET data into site-specific directories..."
for zipfile in "$OUTPUT_DIR"/ICOS_*_FLUXNET_*.zip "$OUTPUT_DIR"/EUF_*_FLUXNET_*.zip; do
    if [[ -f "$zipfile" ]]; then
        echo "  Extracting $(basename "$zipfile")..."
        site=$(basename "$zipfile" | sed -E 's/^(ICOS|EUF)_([^_]+)_.*/\2/')
        unzip_dir="$OUTPUT_DIR/$site"
        mkdir -p "$unzip_dir"
        unzip -o -j "$zipfile" "*.csv" -d "$unzip_dir" 2>/dev/null || true
    fi
done

echo "Done. Extracted files:"
find "$OUTPUT_DIR" -mindepth 2 -maxdepth 2 -name '*.csv' -print
