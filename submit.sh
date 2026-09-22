#!/usr/bin/env bash

#SBATCH --time=23:45:00
#SBATCH --job-name=acclim_targets
#SBATCH --output="_logs/%x-%j.log"

# The full grid: every handled site x every recipe x both models, at the
# manuscript's sampler configuration. This job is only the controller;
# `crew_controller_slurm()` in _targets.R launches the workers as their own
# jobs. See docs/recipes.md for the variables.
#
# Sizing, from the 108 fits the first full run completed. Weighting those by
# the recipe x model grid rather than by what happened to finish first --
# `direct` averages 4077 s against `total`'s 3177 s, and the sample was 60%
# `total` -- puts the mean fit at 3914 s. 1530 fits remain, so about 1660
# core-hours, and 96 workers clear that inside this window even if the
# `cpus_per_task` fix in _targets.R buys nothing. It should buy a good deal:
# 42 of the first run's 68 workers were sampling four chains on the one or two
# CPUs they held on the node the R session landed on.
#
# 96 x 4 = 384 CPUs, about 3% of the `day` partition, and `seconds_idle` hands
# them back through the tail of the run rather than holding them to the end.
export THERMAL_SITES=all
export THERMAL_RECIPES=all
export THERMAL_MODELS=total,direct
export THERMAL_FIT=full
export THERMAL_SLURM_WORKERS=96
# At or above this script's own --time; see the note in _targets.R.
export THERMAL_SLURM_MINUTES=1425

mkdir -p _logs
pixi run targets
