review _agent-docs/*.html for some background.

then, implement workflows/01_02c_filter_high_quality_night_respiration_ICOS.R that follows the general logic of workflows/01_02b_filter_high_quality_night_respiration_AmeriFlux.R but is adapted to use *only* ICOS data (not ICOS + FLUXNET, like the current Euroflux implementation), as downloaded by workflows/93-download-icos.py.

test this against FR-FBn by running 01_01 and 01_02 with FR-FBn (using the newly refactored code that takes only one site).
also, test it against one of the other euroflux sites; you may need to download its data first using download-icos.

document your implementation in _agent-docs/process-icos.html.
