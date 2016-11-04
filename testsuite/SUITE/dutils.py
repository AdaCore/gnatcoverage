# ***************************************************************************
# **                         DUMPING UTILITY functions                     **
# ***************************************************************************

# This module exposes utility functions to dump and reload persistent data,
# in either pickle or json format.

# ***************************************************************************

# --------------------------------
# -- pickle oriented facilities --
# --------------------------------

import pickle

def pdump_to(filename, o):
    with open (filename, 'wb') as f:
        pickle.dump (o, f)
        
def pload_from(filename):
    with open (filename, 'rb') as f:
        return pickle.load (f)

# ------------------------------
# -- json oriented facilities --
# ------------------------------

import json, time, re

def jdump_to(filename, o):
    with open (filename, 'w') as f:
        json.dump (o, f)
        
def jload_from(filename):
    with open (filename, 'r') as f:
        return json.load (f)

def host_string_from(host):
    """Return a textual version of the relevant info in HOST,
    a Env().host kind of object."""

    os_name = host.os.name.capitalize()
    os_version = host.os.version

    # 2008[R2] or 2012[R2] for Windows conveys Server editions

    if os_name == 'Windows' and re.match(os_version, "2008|2012"):
        os_version = "Server " + os_version

    return ' '.join ((os_name, os_version))

def time_string_from(stamp):
    """Return a textual version of the timestamp in STAMP,
    a time.localtime() kind of object."""

    return time.strftime ("%a %b %d, %Y. %H:%M", stamp)
