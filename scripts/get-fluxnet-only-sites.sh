#!/usr/bin/env bash

SNAPSHOT=$(find data-raw -name fluxnet_shuttle_snapshot_*.csv | sort | tail -n1)

bash workflows/92-download-fluxnet.sh \
  --snapshot "$SNAPSHOT" \
  --sites \
  CH-Lae \
  CH-Aws \
  CH-Fru \
  CZ-RAJ \
  CZ-Stn \
  DE-Gri \
  DE-Hai \
  DE-Obe \
  DE-Akm \
  DE-Hte \
  DE-RuC \
  DE-SfS \
  ES-LJu \
  FI-Sod \
  FR-Pue \
  IL-Yat \
  IT-SRo \
  RU-Fyo

# Missing sites:
# ZA-Kru
