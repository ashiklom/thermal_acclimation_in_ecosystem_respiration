#!/usr/bin/env bash

#SBATCH --time=12:30:00
#SBATCH --job-name=acclim_targets
#SBATCH --output="_logs/%x-%j.log"

pixi run targets
