# configurable options of the genrest module

# when this constant is true, genrest tries to deduce the artifact type of a
# folder that doesn't have content .rst file by looking a the subartifacts.
# In case of False, the processing stops with error if such a folder is
# encountered.

ALLOW_UNCLASSIFIED_FOLDERS = False

# a list of warning type patters that should be suppressed
# if there is a match, then genrest computes only the stats

SUPRESS_MSG_TYPES = ["SRC_.*", "MISSING_DESCRITPTION_TEXT"]
