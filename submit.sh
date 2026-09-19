#!/usr/bin/env bash

#SBATCH --time=12:30:00
#SBATCH --job-name=acclim_targets
#SBATCH --output="_logs/%x-%j.log"

# The full grid: every handled site x every recipe x both models, at the
# manuscript's sampler configuration. This job is only the controller;
# `crew_controller_slurm()` in _targets.R launches the workers as their own
# jobs (20 of them, 12 h each). See docs/recipes.md for the variables.
export THERMAL_SITES=all
export THERMAL_RECIPES=all
export THERMAL_MODELS=total,direct
export THERMAL_FIT=full

mkdir -p _logs
pixi run targets
