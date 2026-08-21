write an R script called workflows/91-download-ameriflux.R that downloads the BADM-BASE data for the sites in data/site_info.csv into `data-raw/`. the script should optionally be able to take a comma-separated list of ameriflux site names and then will only download those sites. it should also take an optional path to a site_info.csv file that contains the full list of sites. without arguments, it should default to reading the site list from data/site_info.csv. the script should also take a boolean `--overwrite` flag argument; if not set (default), then skip site data that are already present in `data-raw`; if set, download and overwrite the existing files.

once the script is written, test it by downloading AMF_US-Kon (into data-raw). inspect the contents of the resulting zip file and ensure that they generally match the contents of Demo_code_data_for_1site/AMF_US-Kon_BASE-BADM_5-5.zip. if the files are identical, say so. if the files are similar and should be interoperable, but do not agree exactly in MD5 hash or similar, say so.

next, download the data for the sites to which workflows/01_01_estimate_soil_temperature_at_some_sites.R applies (estimate_Ts == 'YES'), run that script, and confirm that it completes without errors.

ask me clarifying questions as needed.
