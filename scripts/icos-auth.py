#!/usr/bin/env python

# One-time: store ICOS Carbon Portal credentials for icoscp_core.
from icoscp_core.icos import auth
auth.init_config_file()
