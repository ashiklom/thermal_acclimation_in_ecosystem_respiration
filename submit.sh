#!/usr/bin/env bash
# Run the targets pipeline on YCRC Bouchet: `sbatch submit.sh` from the
# repository root. This job is only the targets controller; crew launches
# each worker as its own Slurm job (R/controllers.R). Override any setting
# below on the command line, e.g.
#   sbatch --export=ALL,TAS_SITES=dev,TAS_FIT_PROFILE=fast submit.sh
#SBATCH --job-name=v2-targets
#SBATCH --partition=day
#SBATCH --time=23:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --output=_logs/%x-%j.log

set -euo pipefail
export TAS_SITES="${TAS_SITES:-all}"
export TAS_FIT_PROFILE="${TAS_FIT_PROFILE:-full}"
export TAS_WORKERS="${TAS_WORKERS:-60}"
export TAS_SLURM_PARTITION="${TAS_SLURM_PARTITION:-day}"
export TAS_SLURM_MINUTES="${TAS_SLURM_MINUTES:-1380}"
# one BLAS/OpenMP thread per process (see R/controllers.R)
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1

mkdir -p _logs
echo "sites=$TAS_SITES profile=$TAS_FIT_PROFILE workers=$TAS_WORKERS commit=$(git rev-parse --short HEAD)"
~/.pixi/bin/pixi run Rscript -e 'targets::tar_make()'
