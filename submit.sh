#!/usr/bin/env bash

#SBATCH --time=23:45:00
#SBATCH --job-name=acclim_targets
#SBATCH --output="_logs/%x-%j.log"

# The full grid: every site x every recipe x both models, at the manuscript's
# sampler settings. This job is only the controller; `crew_controller_slurm()`
# in _targets.R launches the workers as their own jobs. Sizing and the reasons
# behind each setting: docs/running-on-ycrc.md.
export THERMAL_SITES=all
export THERMAL_RECIPES=all
export THERMAL_MODELS=total,direct
export THERMAL_FIT=full
export THERMAL_SLURM_WORKERS=96
# At or above this script's own --time.
export THERMAL_SLURM_MINUTES=1425

# pthreads OpenBLAS reads the node's core count, not this job's one CPU.
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1

mkdir -p _logs
pixi run targets
