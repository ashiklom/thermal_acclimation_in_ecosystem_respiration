#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.13"
# dependencies = ["icoscp"]
# ///

from icoscp_core.icos import auth
auth.init_config_file()
