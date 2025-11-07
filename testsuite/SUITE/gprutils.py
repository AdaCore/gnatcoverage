import os
from collections import namedtuple

from SUITE.cutils import contents_of
from SUITE.tutils import gpr_emulator_package


def gprdep_for(reldir, wd):
    """
    Quick facility to help tests exercising GPR trees, in a setup like:

    Tree/ component1
                /src
          ...
          componentN   (reldir, Relative Directory from wd)
                /src

          template.gpr
          Test1/       (wd, test Working Directory)
              test.py

    where test.py will construct its own GPR, with dependencies on sub-GPRs
    that it generates for some components.

    Return a fully qualified GPR dependency item.
    """

    # The local project file we create will be named after both the location
    # where it's store (from reldir) and the test that instantiates it (from
    # wd). The test id part is then reused to name the object directory, to
    # make sure that each test operating with a given relative dir has its own
    # object dir there and can run in parallel with others.

    locid = os.path.basename(reldir.rstrip("/"))
    testid = os.path.basename(wd.homedir.rstrip("/"))

    prjname = "%s_%s" % (locid, testid)
    gprdep = os.path.join(wd.homedir, reldir, prjname)

    with open(gprdep + ".gpr", "w") as gprfile:
        gprfile.write(
            contents_of(os.path.join(wd.homedir, "../template.gpr"))
            % {
                "prjname": prjname,
                "objdir": "obj_" + testid,
                "pkg_emulator": gpr_emulator_package(),
            }
        )
    return gprdep


Csw = namedtuple("Csw", "cmd switches")


def __gprattr(attrname, value, aslist):
    """

    One project attribute definition string, for an attribute named ATTRNAME,
    with the provided attribute VALUE, to be set as an attribute list-value or
    not according to ASLIST. The definition string degrades into a mere comment
    for value == None. Not for an empty list or string.
    """
    if value is None:
        return "-- empty %s" % attrname

    elif aslist:
        return "for %s use (%s);" % (
            attrname,
            ",".join('"%s"' % v for v in value),
        )
    else:
        return 'for %s use "%s";' % (attrname, value)


def __gpr_uattr(value, for_list, to_exclude):
    """
    One attribute definition string, for a unit set kind of attribute, one of
    (Units, Units_List, Excluded_Units, Excluded_Units_List). The FOR_LIST
    argument qualifies the attribute name we aim at. When True, we typically
    have a single list-filename argument.
    """
    return __gprattr(
        attrname="%(prefix)s%(kind)s"
        % {
            "prefix": "Excluded_" if to_exclude else "",
            "kind": "Units_List" if for_list else "Units",
        },
        value=value,
        aslist=not for_list,
    )


def gprcov_for(
    units_in=None, ulist_in=None, units_out=None, ulist_out=None, switches=()
):
    """
    Compute and return the text of a Coverage GPR package from:

    * provided units or lists to include or exclude
    * default_switches to install

    This returns the full Coverage package for a project file, with attribute
    definition strings for Units, Units_List, Excluded_Units,
    Excluded_Units_List and Switches, each boiling down to a mere comment if
    the corresponding argument passed here is None. For SWITCHES, we expect a
    command->switches sequence of ("command", [options]) Csw tuples.
    """
    return "\n".join(
        [
            "package Coverage is",
            # Units
            __gpr_uattr(for_list=False, to_exclude=False, value=units_in),
            # Excluded_Units
            __gpr_uattr(for_list=False, to_exclude=True, value=units_out),
            # Units_List
            __gpr_uattr(for_list=True, to_exclude=False, value=ulist_in),
            # Excluded_Units_List
            __gpr_uattr(for_list=True, to_exclude=True, value=ulist_out),
        ]
        + [
            # Switches (CMD)
            __gprattr(
                attrname='Switches ("%s")' % csw.cmd,
                value=csw.switches,
                aslist=True,
            )
            for csw in switches
        ]
        + ["end Coverage;"]
    )


class GPRswitches:
    """
    Handler for GPR-related switches for gnatcov commands.

    A class to materialize GPR related instructions for gnatcov commands as
    state variables instead of option strings, which facilitates the
    development of tests where computed variations of some of the controls
    need to be exercised.

    Facilities are provided to expose the switches that require coordination
    with the build step that precedes the gnatcov invocations (e.g. --subdirs).
    """

    def __init__(
        self,
        root_project,
        projects=None,
        units=None,
        no_subprojects: bool = False,
        externally_built_projects: bool = False,
        relocate_build_tree: str | None = None,
        root_dir=None,
        xvars=None,
        subdirs=None,
    ):
        """
        :param str root_project: Root project to consider (-P argument).
        :param list[str] projects: Optional list of projects for units of
           interest (--project argument).
        :param list[str] units: Optional list of names of units of interest
           (--units argument).
        :param bool no_subprojects: Whether to process only projects specified
            through -P/--projects (--no-subprojects option). Otherwise, process
            closures of project dependencies.
        :param bool externally_built_projects: Whether to process externally
            built projects (--externally-built-projects).
        :param list[(str,str)] xvars: Optional list of (varname, value) tuples
            for scenario variables (--X arguments).
        :param str subdirs: Optional --subdirs argument.
        """
        self.root_project = root_project
        self.projects = projects or []
        self.units = units or []
        self.no_subprojects = no_subprojects
        self.externally_built_projects = externally_built_projects
        self.relocate_build_tree = relocate_build_tree
        self.root_dir = root_dir
        self.xvars = xvars or []
        self.subdirs = subdirs

    @property
    def build_switches(self):
        """
        Switches that would be valid for gprbuild as well and which
        should be used in coordination, as a list of strings.
        """
        switches = []

        for v in self.xvars:
            switches.append("-X{}={}".format(v[0], v[1]))

        if self.subdirs:
            switches.append("--subdirs={}".format(self.subdirs))

        if self.relocate_build_tree:
            switches.append(
                "--relocate-build-tree={}".format(self.relocate_build_tree)
            )

        if self.root_dir:
            switches.append("--root-dir={}".format(self.root_dir))

        return switches

    @property
    def cov_switches(self):
        """
        The switches for gnatcov commands, as a list of strings.
        """
        switches = ["-P{}".format(self.root_project)]

        for p in self.projects:
            switches.append("--projects={}".format(p))

        for u in self.units:
            switches.append("--units={}".format(u))

        if self.no_subprojects:
            switches.append("--no-subprojects")

        if self.externally_built_projects:
            switches.append("--externally-built-projects")

        return switches + self.build_switches
