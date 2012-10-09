import os
from SUITE.cutils import contents_of

# ----------------
# -- gprdep_for --
# ----------------

# Quick facility to help tests exercising GPR trees,
# in a setup like
#
# Tree/ component1
#             /src
#       ...
#       componentN   (reldir, Relative Directory from wd)
#             /src
#
#       template.gpr
#       Test1/       (wd, test Working Directory)
#           test.py
#
# where test.py will construct its own GPR,
# with dependencies on sub-GPRs that it generates for
# some components.
#
# Return a fully qualified GPR dependency item

def gprdep_for (reldir, wd):

    # The local project file we create will be named after both the location
    # where it's store (from reldir) and the test that instantiates it (from
    # wd). The test id part is then reused to name the object directory, to
    # make sure that each test operating with a given relative dir has its own
    # object dir there and can run in parallel with others.

    locid = os.path.basename (reldir.rstrip ('/'))
    testid = os.path.basename (wd.homedir.rstrip ('/'))

    prjname = "%s_%s" % (locid, testid)
    gprdep = os.path.join (wd.homedir, reldir, prjname)

    with open (gprdep + ".gpr", 'w') as gprfile:
        gprfile.write (
            contents_of (os.path.join (wd.homedir, "../template.gpr")) % {
                "prjname" : prjname,
                "objdir" : "obj_" + testid
                }
            )

    return gprdep

# ----------------
# -- gprcov_for --
# ----------------

# Compute and return the text of a Coverage GPR package from
# * provided units or lists to include or exclude
# * default_switches to install

def __gprattr (attrname, value, aslist):
    """One project attribute definition string, for an attribute named
    ATTRNAME, with the provided attribute VALUE, to be set as an attribute
    list-value or not according to ASLIST. The definition string degrades into
    a mere comment for value == None. Not for an empty list or string."""

    if value is None:
        return "-- empty %s" % attrname

    elif aslist:
        return "for %s use (%s);" % (
            attrname, ','.join (['\"%s\"' % v for v in value])
            )
    else:
        return "for %s use \"%s\";" % (attrname, value)

def __gpr_uattr (value, for_list, to_exclude):
    """One attribute definition string, for a unit set kind of attribute, one
    of (Units, Units_List, Excluded_Units, Excluded_Units_List). The FOR_LIST
    argument qualifies the attribute name we aim at. When True, we typically
    have a single list-filename argument."""

    return __gprattr (
        attrname = "%(prefix)s%(kind)s" % {
            "prefix": "Excluded_" if to_exclude else "",
            "kind": "Units_List" if for_list else "Units"
            },
        value  = value,
        aslist = not for_list)

def gprcov_for (
    units_in=None,
    ulist_in=None,
    units_out=None,
    ulist_out=None,
    def_switches=None
    ):
    """The full Coverage package for a project file, with attribute
    definition strings for Units, Units_List, Excluded_Units, Excluded_Units_List
    and Default_Switches, each boiling down to a mere comment if the corresponding
    argument passed here is None."""

    return '\n'.join ([
            "package Coverage is",

            __gpr_uattr ( # Units
                for_list = False,
                to_exclude = False,
                value = units_in),

            __gpr_uattr ( # Excluded_Units
                for_list = False,
                to_exclude = True,
                value = units_out),

            __gpr_uattr ( # Units_List
                for_list = True,
                to_exclude = False,
                value = ulist_in),

            __gpr_uattr ( # Excluded_Units_List
                for_list = True,
                to_exclude = True,
                value = ulist_out),

            __gprattr ( # Default_Switches
                attrname = "Default_Switches",
                value = def_switches,
                aslist = True),

            "end Coverage;"])
