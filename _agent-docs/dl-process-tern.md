review `_agent-docs/*.html` for context. then, implement a `workflows/94-download-tern` script (in either R, python, or bash -- whatever makes the most sense) that downloads TERN data in the format expected for workflow `01_01` and `01_02*`.

Try this: https://github.com/ternaustralia/terndata.flux (note that it's in Python, and works with Xarray objects. Would need a translation layer to get to BADM). So probably a python script makes sense, in the `uv script` self-contained style.

then, implement 01_02d that is analogous to the Ameriflux and ICOS-specific scripts but for TERN.

when done, test steps 01_01 and 01_02d on 5 representative TERN sites that give you as much code coverage as possible.
