#!/usr/bin/env bash

# Periodic update: ask the providers what they publish, and run the pipeline
# only if that changed anything. Meant for cron, or scrontab on YCRC:
#
#   # scrontab -e
#   #SCRON --time=00:30:00
#   #SCRON --job-name=acclim_scan
#   #SCRON --output=/path/to/repo/_logs/scan-%j.log
#   30 4 * * * /path/to/repo/scripts/scan-and-run.sh
#
# On a day with nothing new it scans (about 40 s, mostly the fluxnet-shuttle
# listing), finds `tar_outdated()` empty, and exits. Otherwise it hands the
# run to `submit.sh` under SLURM, or runs `pixi run targets` in the foreground
# where there is no `sbatch`. targets then rebuilds only the sites whose
# catalogue rows changed -- see R/remote-catalog.R.
#
# The grid is `submit.sh`'s (`THERMAL_SITES=all` and so on), because that is
# the run the outdated check has to describe. Any `THERMAL_*` variable already
# set in the environment wins, which is how to exercise this on a laptop:
#
#   THERMAL_SITES=dev THERMAL_RECIPES=dev THERMAL_FIT=fast scripts/scan-and-run.sh
#
#   --scan-only   update data-raw/remote_catalog.csv and report, run nothing
#   --no-scan     skip the scan; decide from the catalogue already on disk

set -euo pipefail

cd "$(dirname "$0")/.."

SCAN=true
RUN=true
for arg in "$@"; do
    case $arg in
        --scan-only) RUN=false ;;
        --no-scan) SCAN=false ;;
        -h|--help) sed -n '3,25p' "$0"; exit 0 ;;
        *) echo "Unknown option: $arg" >&2; exit 2 ;;
    esac
done

log() { echo "[$(date '+%F %T')] $*"; }

# submit.sh's grid, without overriding anything the caller set.
while IFS='=' read -r var val; do
    var=${var#export }
    if [[ -z "${!var:-}" ]]; then
        export "$var=$val"
    fi
done < <(grep -E '^export THERMAL_[A-Z_]+=' submit.sh)

# One of these at a time. `mkdir` is atomic everywhere, unlike `flock`, which
# macOS does not ship.
mkdir -p _logs
LOCK=_logs/scan-and-run.lock
if ! mkdir "$LOCK" 2>/dev/null; then
    log "another scan-and-run holds $LOCK; exiting"
    exit 0
fi
trap 'rmdir "$LOCK"' EXIT

# A submitted pipeline outlives this script, and a second `tar_make()` on the
# same store would corrupt it -- and, mid-run, the outdated check below is
# never empty.
if command -v squeue >/dev/null 2>&1; then
    if [[ -n "$(squeue -h -u "$USER" -n acclim_targets -o %i)" ]]; then
        log "pipeline job acclim_targets is already queued or running; exiting"
        exit 0
    fi
fi

if $SCAN; then
    log "scanning providers"
    pixi run uv run scripts/check-data-updates.py --catalog data-raw/remote_catalog.csv
fi

log "checking for outdated targets (sites=$THERMAL_SITES recipes=${THERMAL_RECIPES:-dev} fit=${THERMAL_FIT:-full})"
outdated=$(pixi run Rscript -e 'cat("\nN_OUTDATED=", length(targets::tar_outdated(reporter = "silent")), "\n", sep = "")' \
    | sed -n 's/^N_OUTDATED=//p')
if [[ -z "$outdated" ]]; then
    log "could not determine outdated targets" >&2
    exit 1
fi
if [[ "$outdated" == 0 ]]; then
    log "no new data; nothing to run"
    exit 0
fi
log "$outdated target(s) outdated"

if ! $RUN; then
    exit 0
fi
if command -v sbatch >/dev/null 2>&1; then
    log "submitting submit.sh"
    sbatch submit.sh
else
    log "running the pipeline in the foreground"
    pixi run targets
fi
