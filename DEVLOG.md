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
- Figuring out the workflow. Start from the `brms` model and work backwards.
- Two models: total TAS (simple) and direct TAS (more complicated)
- total TAS:
    - Formula: `frmu <- NEE ~ exp(alpha * TS + beta*TS^2) * C0`
    - Parameters: `param <- alpha + beta + C0 ~ 1`
    - Always call model as: `brms::bf(frmu, param, nl = TRUE)`
    - Prior workflow (for each site):
        - Start with default prior (note: _no cross-site dependence!_)
        - Update first prior based on `gsl_nls` results
- Working on refactor in `R/total_tas.R`

## 2026-09-11

I need to carefully revisit the data preparation steps (workflow step 1 -- 01-01 and 01-02). The resulting data must never have NA values for NEE, TS, or other values associated with the regression. While I'm at it, I can clean up some other stuff too.

Soil temperature correction (workflow step 1) is used to set `a$TS_F_MDS_1`

```r
a$TS_F_MDS_1 <- df_TS$TS_pred
a$TS_F_MDS_1_QC[is.na(a$TS_F_MDS_1_QC) | a$TS_F_MDS_1_QC == 3] <- 2
```

`gStart / gEnd` logic is strange. For one, the original code doesn't make sense (see inline TODO). Second, for a (southern hemisphere) site like `AU-Tum`, we are perpetually in the growing season according to 

Southern hemisphere logic is hard. Let me start with conceptually simpler northern hemisphere site (ICOS, to keep the logic simpler without the Ameriflux ustar filtering).

`NA` values can still slip through the cracks of the current implicit filters for ICOS and TERN data! This is because we don't explicitly check for `!is.na(NEE)` or `!is.na(TS)` --- rather, we rely on the QC flags being set. But there are situations where the QC flag for some of the data is 0 (good measured data) but the values are `NA`. So, we have to filter for `NA` for predictor and response variables explicitly.

TERN and ICOS data pre-processing should be complete. Ameriflux still needs doing --- however, the existing helpers make it a lot easier. Also note the annotation of the ustar filtering in the existing code; that also makes it easy to see where the standard workflow picks up.

Next:
- Ameriflux pre-processing
- FLUXNET pre-processing
- Revisit growing season detection stuff, especially for southern hemisphere. See _agent-docs/better-growing-season.md for a potentially better implementation. The session is "find long gap years logic..." in the `thermal_acclimation-original` folder.
- Run the pre-preprocessing for all the sites in targets
- Do the model fits.

## 2026-09-14

- Ameriflux
    - Base case is complete. Still need to sort out a few to-do's; most importantly, soil temperature logic.
- Non-Ameriflux
    - `measured` is just `a` filtered against a bunch of columns. May as well rename everything in this step.
    - `ac` is just `a` but filtered to good years.
- In general, the columns coming out of the preparation scripts should be standardized already. Non-Ameriflux `measured `uses custom filtering based on QC flags, while Ameriflux comes out already clean. So, out of my first if-else block, produce `ac` and `measured`.
- `ac` --> `ac_final` and `measured` --> `measured_final` is just filtering to good years and then column selection. If I standardize the column names for `ac` and `measured` between Ameriflux vs. others, this should be trivial.
- I think I now have a working Ameriflux workflow (for the general case; no soil temperature yet), but it produces some missing `TA` values. Do these actually need to be non-NA?
    - AI says: `!is.na(TA)` check is unnecessary. The variables we need are `NEE`, `TS` (for the total TAS), `SWC`, and `NEE_daytime` (for direct TAS).
- Implemented soil temperature prediction.
- Implemented ICOS, TERN, and FLUXNET data reads. All of these seem to be working now.
    - IT-Noe has "no non-missing arguments to min" warning. Double check.
- Next:
    - Add target for fitting models.
    - Run model fits on existing sites.
    - Figure out southern hemisphere logic.
