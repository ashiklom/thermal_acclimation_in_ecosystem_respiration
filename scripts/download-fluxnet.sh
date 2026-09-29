#!/bin/bash

# Download FLUXNET-Archive data with the fluxnet-shuttle CLI, and extract each
# site's tables into data-raw/FLUXNET/<site>/. Run inside the pixi environment:
#
#   pixi run bash scripts/download-fluxnet.sh --sites FR-Pue CZ-RAJ
#
# Whether a site needs downloading at all is decided by the caller
# (`download_site()` in R/download.R); this always fetches what it is given.

set -euo pipefail

SNAPSHOT_FILE=""
OUTPUT_DIR="data-raw/FLUXNET"
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
            # Accepted for the R caller; the shuttle always re-downloads.
            shift
            ;;
        -h|--help)
            echo "Usage: $0 --sites SITE1 [SITE2 ...] [--snapshot FILE]"
            echo ""
            echo "  -s, --sites SITE1 SITE2  Site IDs to download (required)"
            echo "  -f, --snapshot FILE      Snapshot CSV (default: the newest data-raw/fluxnet_shuttle_snapshot_*.csv)"
            echo "  -h, --help               Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [[ ${#SITES[@]} -eq 0 ]]; then
  echo "Error: --sites is required." >&2
  exit 2
fi

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
    fluxnet-shuttle listall -o data-raw
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
CMD=(fluxnet-shuttle download --snapshot-file "$SNAPSHOT_FILE" --output-dir "$OUTPUT_DIR" --quiet)

CMD+=(--sites "${SITES[@]}")

# Run download
echo "Downloading FLUXNET data..."
"${CMD[@]}"

# Unzip each requested site's archive into its site-specific FLUXNET
# directory. Only the sites just downloaded: re-extracting every archive in
# the directory rewrites tables that have not changed, which is slow and makes
# every other site's files look touched.
echo "Extracting FLUXNET data into site-specific directories..."
for site in "${SITES[@]}"; do
    # Any network prefix (ICOS_, EUF_, AMF_, ...), as product_local_paths() in
    # R/remote-catalog.R matches them.
    for zipfile in "$OUTPUT_DIR"/*_"${site}"_FLUXNET_*.zip; do
        if [[ -f "$zipfile" ]]; then
            echo "  Extracting $(basename "$zipfile")..."
            unzip_dir="$OUTPUT_DIR/$site"
            mkdir -p "$unzip_dir"
            unzip -o -j "$zipfile" "*.csv" -d "$unzip_dir" 2>/dev/null || true
        fi
    done
done

echo "Done. Extracted files:"
find "$OUTPUT_DIR" -mindepth 2 -maxdepth 2 -name '*.csv' -print
