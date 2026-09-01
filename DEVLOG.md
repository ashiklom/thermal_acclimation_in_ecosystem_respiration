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
    - [ ] Simplify FLUXNET path logic in `process_site`
    - [ ] Check if all TERN sites have soil temperature (for future downloads)
    - [ ] Move site-specific column name logic out of code and into `site_info` --- look for `US-Ha2`
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


