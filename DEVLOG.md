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
