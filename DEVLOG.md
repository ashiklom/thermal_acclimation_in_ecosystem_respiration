# Developer log

## To-do

- [ ] Simplify file path logic. There is an unambiguous mapping between site info `source` and directory path. No need to search for data anymore. Maybe make a utility function.
- [X] Single script (or collection of scripts) to download data. Filter `data-core/site_info.csv` for that specific data type; then, run the corresponding download script for those sites.
    - Done for FLUXNET -- `scripts/get-fluxnet-only-sites.sh`
        - [X] Merge this logic into `workflows/92-download-fluxnet.sh`
    - Ameriflux script already does this
    - [X] TERN --- revised to look for `source == "TERN"`
    - [X] ICOS
    - [X] Shared wrapper --- using `pixi` tasks for this.
- [ ] Try running the entire workflow for a single site, using the existing prior.
- [ ] Repeat above for a few more sites.
- [ ] Script to check if new data needs to be downloaded or if we have it already (maybe based on MD5 hashes?)
    - Ameriflux --- check the `amerifluxr` has some way to query metadata.
    - TERN, ICOS --- check the download functions and APIs they use
    - fluxnet-shuttle --- can use the fluxnet shuttle `listall` CSV file.

- `workflows/01_01_estimate_soil_temperature_at_some_sites.R`
    - [ ] Replace `read.csv` with `readr::read_csv` for robustness and performance
    - [X] Simplify FLUXNET path logic in `process_site`
    - [ ] Check if all TERN sites have soil temperature (for future downloads)
    - [ ] Move site-specific column name logic out of code and into `site_info`
        - US-Ha2 still has custom column names
        - Site-specific logic for FLUXNET sites DE-Hte, FR-Bil, FR-Pue
    - [ ] Use new `summarize(..., .by)` syntax.

## 2026-08-28

- Improved workflow for data downloads. Added pixi tasks.
- Now, working through `workflows/01_01_estimate_soil_temperature_at_some_sites.R`
    - Added `R/utils.R` with `site_info` function for cleanly and robustly getting site information.
    - Cleaned up TERN-specific section.
    - `process_site`
        - TERN processing is simplified because it already includes soil temperature (universally?? Or just for AU-Tum, the one TERN site we have?
        - For all others, there is branching data reading (to correctly assign column names, etc.). Then, estimate TS using a few different methods.
            - NOTE: I moved some more site-specific information about how to estimate TS out of the code and into `site_info.csv`.
        - ICOS file processing is straightforward. Confirmed with an example.
        - For FLUXNET, there's some complicated file-combining logic. This can be simplified.
        - Made some progress on cleaning up Ameriflux logic, but more to do.
- NEXT:
    - Continue cleaning up Ameriflux logic in `process_site`
    - Clean up FLUXNET logic in `process_site`
    - Remove extraneous code in `01-01`
    - Move on to `01-02` scripts

## 2026-08-31

- Tiny amount of cleanup of `workflows/01_01_estimate_soil_temperature_at_some_sites.R`

## 2026-09-01

- Removed dirty file path logic in 01-01. Now the structure is nice and clean. Good enough for now; can revisit other actions later.
- AI refactored `workflows/01_02_filter_high_quality_night_respiration.R` into a single script that internally dispatches based on site identity and source.
    - Notes on how it did this are here: `_agent-docs/workflow-02-02-refactor.html`
- AI wrote a testing framework that lives here: `_agent-docs/all_site_tests.html`.
    - Discovered that we are missing Ameriflux metadata. `workflows/96-download-aanet.R` downloads that.
    - Discovered that we are missing ERA5 daily SWC data, which is a prerequisite for workflow step 02. `workflows/97-download-era5-swc.py` gets that.
    - Also missing `wc2.1_2.5m_tmin*` (presumably, WorldClim data?)
        - Gemini says this is probably "WorldClim version 2.1 at a spatial resolution of 2.5 arc-minutes (~4.5 km at the equator). The tmin string indicates these are minimum temperature variables"
    - Also missing `GSOCmap1.5.0.tif`
        - Gemini says this is probably "Global Soil Organic Carbon (GSOC) map (version 1.5.0). Developed by the FAO (Food and Agriculture Organization) and the Intergovernmental Technical Panel on Soils (ITPS)"

## 2026-09-05

- Looking into exact where data dependencies are needed
    - ERA5 SWC is needed for all sites where `site_info$SWC_use == 'NO'`. There are 66 such sites.
    - `wc2.1_2.5m_tmin.tif` is only needed for `workflows/04_01_get_future_night_soil_temperature_change.R`

## 2026-09-09

- Created download ERA5 script. Using the ARCO Zarr makes this really fast and easy.
- Added `_creds.toml` docs to README.
