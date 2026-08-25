Clean up the paths inside `data-raw` and scripts that put data in there.
I probably want something much simpler and obvious like `data-raw/{Ameriflux,ICOS,TERN}`.
If specific sites have multiple files associated with them, put those sites in subdirectories --- e.g.,
`data-raw/Ameriflux/US-CMW/<files go here>`
Let me know if just that simple approach is insufficient for the complexity of the ata.

Do not re-download existing data. Just move existing files.

modify the download scripts.
also modify all the workflow R scripts that look for data in these paths.

present me with an implementation plan before proceeding.
