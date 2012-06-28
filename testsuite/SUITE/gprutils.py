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

# Compute and return the text of a Coverage GPR package
# from provided units or lists to include or exclude.

def __gprattrname (for_list, to_exclude):
    return "%(prefix)s%(kind)s" % {
        "prefix": "Excluded_" if to_exclude else "",
        "kind": "Units_List" if for_list else "Units"
        }

def __gprattr (value, for_list, to_exclude):
    attrname = __gprattrname (for_list=for_list, to_exclude=to_exclude)
    return (
        ("for %s use \"%s\";" % (attrname, value)
         ) if (value is not None and for_list)
        else
        ("for %s use (%s);" % (
                attrname, ','.join (['\"%s\"' % v for v in value])
                )
         ) if value is not None and not for_list
        else
        ("-- empty %s" % attrname)
        )

def gprcov_for (units_in, ulist_in, units_out, ulist_out):
    return '\n'.join ([
            "package Coverage is",
            __gprattr (
                for_list = False, to_exclude = False, value = units_in),
            __gprattr (
                for_list = False, to_exclude = True, value = units_out),
            __gprattr (
                for_list = True, to_exclude = False, value = ulist_in),
            __gprattr (
                for_list = True, to_exclude = True, value = ulist_out),
            "end Coverage;"])
