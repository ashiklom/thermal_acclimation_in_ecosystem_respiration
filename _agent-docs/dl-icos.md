write a download script in workflows/93-download-icos to download ICOS flux tower data. we want the level 2 data. this should have a similar interface to the other download scripts. if it doesn't need authentication, don't include arguments for it. the script should be able to take a list of one or more sites to download.
the list of ICOS sites is here: ~/Downloads/stations.csv.
test the script against the FR-FBn site (see `_agent-docs/icos-sites.html` for some context).
when the data are downloaded, confirm that they work with `workflows/01_02c_filter_high_quality_night_respiration_ICOS.R` (see `_agent-docs/icos-sites.html` for context).

use the icoscp pypi package to download the data. it should already be authenticated via a user-wide auth file; confirm this with basic functionality. write the download script in the `uv script` style (see the workflows/99-icos-auth.py for an example.)

if you can, try to download the raw data in the format expected by the workflows night respiration euroflux script.
if not (e.g., access is directly to pandas data frames or something), pull the data necessary for workflow steps 01_01 and 01_02.

ask me clarifying questions as needed.
