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

import json, time

def jdump_to(filename, o):
    with open (filename, 'w') as f:
        json.dump (o, f)
        
def jload_from(filename):
    with open (filename, 'r') as f:
        return json.load (f)

def host_string_from(host):
    """Return a textual version of the relevant info in HOST,
    a Env().host kind of object."""
    return '-'.join (
        (host.os.name, host.os.kernel_version+'/'+host.os.version))
        
def time_string_from(stamp):
    """Return a textual version of the timestamp in STAMP,
    a time.localtime() kind of object."""

    return time.strftime ("%a %b %d, %Y. %H:%M", stamp)
