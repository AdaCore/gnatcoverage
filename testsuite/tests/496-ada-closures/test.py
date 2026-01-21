"""
Check that gnatcov instruments the same Ada units as gprbuild (i.e. sets of
sources based on closures when relevant).
"""

from __future__ import annotations

import dataclasses
import glob
import os.path

from e3.fs import mkdir, mv

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor

tmp = Wdir("tmp_")


info_file = {"bin": "ALI", "src": "SID"}[thistest.options.trace_mode]


# Mappings to sources and projects created by dataclasses below, indexed by
# filename/project name.
all_sources = {}
all_projects = {}


@dataclasses.dataclass
class Source:
    """Source file in a project."""

    # Source file basename. The contents of the source file is derived from the
    # extension:
    # * .ads: Ada package (must not be a main).
    # * .adb: Ada procedure (must be a main).
    # * .c: C source file that defines a variable (when not a main) or the
    #   "main" function (when a main).
    filename: str

    # Whether this source file must be a main in project files
    is_main: bool = False

    # List of unit dependencies for this source file (for Ada sources only)
    deps: list[str] = dataclasses.field(default_factory=list)

    # List of C dependencies for this source file (for main sources only, and C
    # deps must be non-mains), so that C units are preserved during the link.
    c_deps: list[str] = dataclasses.field(default_factory=list)

    # Unit name (for Ada sources), derived from the filename
    unit_name: str = dataclasses.field(init=False)

    # Source file contents, derived from the filename
    contents: str = dataclasses.field(init=False)

    def __post_init__(self):
        # Each source file must have a unique filename
        assert self.filename not in all_sources, str(self.filename)
        all_sources[self.filename] = self

        # Only mains can have C dependencies
        assert self.is_main or not self.c_deps
        c_deps_symbols = [os.path.splitext(c_dep)[0] for c_dep in self.c_deps]

        unit_name, ext = os.path.splitext(self.filename)
        if ext == ".c":
            assert not self.deps, str(self.filename)
            if self.is_main:
                contents = ""
                for sym in c_deps_symbols:
                    contents += f"extern int {sym};\n"
                contents += "int\nmain (void)\n{"
                for sym in c_deps_symbols:
                    contents += f"\n  {sym} += 1;"
                contents += "\n  return 0;\n}\n"
            else:
                contents = (
                    f"int {unit_name} = 0;"
                    "\nvoid"
                    f"\n{unit_name}_func (void)"
                    "\n{"
                    "\n  return;"
                    "\n}"
                    "\n"
                )
        else:
            # For convenience, always generate units with a WITH clause to make
            # line numbers stable.
            unit_name = unit_name.capitalize()
            deps = [d.capitalize() for d in self.deps] or ["Ada"]
            with_clause = "with {};".format(", ".join(deps))
            if ext == ".ads":
                assert not self.is_main, str(self.filename)
                contents = (
                    f"{with_clause}"
                    "\n"
                    f"\npackage {unit_name} is"
                    "\n   I : Integer := 0;"
                    f"\nend {unit_name};"
                    "\n"
                )
            elif ext == ".adb":
                unit_name = unit_name.capitalize()
                contents = f"{with_clause}\n\nprocedure {unit_name} is"
                for sym in c_deps_symbols:
                    contents += f"\n   {sym} : Integer;"
                    contents += f"\n   pragma Import (C, {sym});"
                contents += "\nbegin"
                if c_deps_symbols:
                    for sym in c_deps_symbols:
                        contents += f"\n   {sym} := {sym} + 1;"
                else:
                    contents += "\n   null;"
                contents += f"\nend {unit_name};\n"
            else:
                raise AssertionError(f"unhandled filename: {self.filename}")

        self.unit_name = unit_name
        self.contents = contents


@dataclasses.dataclass
class Project:
    """Project file."""

    # Project name
    name: str

    # List of source files that belong to this project
    sources: list[Source]

    # GPR dependencies (by project name)
    deps: list[str | Project] = dataclasses.field(default_factory=list)

    # Whether this is a library project
    is_library: bool = False

    # If this is library project, GPR attribute that defines its interface (if
    # any).
    library_interface_attr: str = ""

    # Project filename, derived from the project name
    filename: str = dataclasses.field(init=False)

    @property
    def objdir(self) -> str:
        """Object directory for this project, relative to the project dir."""
        return f"obj/{self.name}"

    @property
    def mains(self) -> list[str]:
        """List of filenames for GPR mains."""
        return [s.filename for s in self.sources if s.is_main]

    def __post_init__(self):
        # Each project must have a unique name
        assert self.name not in all_projects, str(self.name)
        all_projects[self.name] = self

        # Create the source directory and populate it
        srcdir = f"src-{self.name}"
        mkdir(srcdir)
        for s in self.sources:
            with open(f"{srcdir}/{s.filename}", "w") as f:
                f.write(s.contents)

        assert self.is_library or not self.library_interface_attr
        self.filename = gprfor(
            prjid=self.name,
            deps=[d if isinstance(d, str) else d.name for d in self.deps],
            srcdirs=[srcdir],
            objdir=self.objdir,
            mains=self.mains,
            extra=(
                f"""
                for Library_Name use "{self.name}";
                for Library_Dir use "{self.objdir}-lib";
                {self.library_interface_attr}
                """
                if self.is_library
                else ""
            ),
        )


@dataclasses.dataclass
class Test:
    # Root project passed to gnatcov and gprbuild
    root_project: Project

    # If this project has mains, set of source files expected to be part of the
    # xcov report.
    expected_xcov: set[str] | None = None

    # If this project has mains, list of Ada units that are not supposed to be
    # instrumented/compiled: "gnatcov coverage" is expected to warn about
    # missing SIDs/ALIs for them.
    missing_unit_info: set[str] = dataclasses.field(default_factory=set)

    def __post_init__(self):
        if self.root_project.mains:
            assert self.expected_xcov is not None
        else:
            assert self.expected_xcov is None
            assert not self.missing_unit_info


tests = [
    # Start with basic cases: single projects (no dependencies).
    #
    # Ada-only project with no mains: all units are supposed to be compiled.
    Test(
        root_project=Project(
            name="basic1",
            sources=[
                Source("basic1_pkg1.ads"),
                Source("basic1_pkg2.ads"),
            ],
        ),
    ),
    # Ada-only project with a main: units not in its closure are not supposed
    # to be compiled.
    Test(
        root_project=Project(
            name="basic2",
            sources=[
                Source("basic2_extra.ads"),
                Source("basic2_main.adb", is_main=True, deps=["basic2_pkg"]),
                Source("basic2_pkg.ads"),
            ],
        ),
        expected_xcov={"basic2_main.adb", "basic2_pkg.ads"},
        missing_unit_info={"basic2_extra"},
    ),
    # Ada/C project with an Ada main: unit not in the Ada main closure are not
    # supposed to be compiled.
    Test(
        root_project=Project(
            name="basic3",
            sources=[
                Source("basic3_extra.ads"),
                Source("basic3_foo.c"),
                Source(
                    "basic3_main.adb",
                    is_main=True,
                    deps=["basic3_pkg"],
                    c_deps=["basic3_foo.c"],
                ),
                Source("basic3_pkg.ads"),
            ],
        ),
        expected_xcov={"basic3_foo.c", "basic3_main.adb", "basic3_pkg.ads"},
        missing_unit_info={"basic3_extra"},
    ),
    # Ada/C project with both an Ada and a C main: all units are supposed to be
    # compiled.
    Test(
        root_project=Project(
            name="basic4",
            sources=[
                Source("basic4_extra.ads"),
                Source("basic4_main.adb", is_main=True, deps=["basic4_pkg"]),
                Source("basic4_pkg.ads"),
                Source("basic4_foo.c", is_main=True),
            ],
        ),
        expected_xcov={
            "basic4_extra.ads",
            "basic4_main.adb",
            "basic4_foo.c",
            "basic4_pkg.ads",
        },
    ),
    # Now, go on with library projects.
    #
    # In the absence of a library interface, all units should be instrumented.
    Test(
        root_project=Project(
            name="lib1",
            sources=[Source("lib1_pkg1.ads"), Source("lib1_pkg2.ads")],
            is_library=True,
        ),
    ),
    # With a library interface that contains only Ada units, only Ada units in
    # the closure should be instrumented.
    Test(
        root_project=Project(
            name="lib2",
            sources=[
                Source("lib2_pkg1.ads", deps=["lib2_pkg3"]),
                Source("lib2_pkg2.ads"),
                Source("lib2_pkg3.ads"),
            ],
            is_library=True,
            library_interface_attr='for Interfaces use ("lib2_pkg1.ads");',
        )
    ),
    # Even with a library interface that contains non-Ada files, only Ada units
    # in the closure should be instrumented.
    Test(
        root_project=Project(
            name="lib3",
            sources=[
                Source("lib3_pkg1.ads", deps=["lib3_pkg4"]),
                Source("lib3_pkg2.ads"),
                Source("lib3_pkg3.c"),
                Source("lib3_pkg4.ads"),
            ],
            is_library=True,
            library_interface_attr=(
                'for Interfaces use ("lib3_pkg1.ads", "lib3_pkg3.c");'
            ),
        )
    ),
    # Now add dependencies.
    #
    # First, check with an Ada main: only its Ada closures are supposed to be
    # instrumented, even with C sources in the dependencies.
    Test(
        root_project=Project(
            name="deps1_root",
            sources=[
                Source(
                    "deps1_main.adb",
                    is_main=True,
                    deps=[
                        "deps1_pkg1",
                        "deps1_pkg2",
                        "deps1_pkg4",
                        "deps1_pkg6",
                    ],
                    c_deps=["deps1_pkg3.c"],
                ),
            ],
            deps=[
                # For this simple project, only Ada units in the closure of an
                # Ada main must be built.
                Project(
                    name="deps1_lib1",
                    sources=[
                        Source("deps1_extra1.ads"),
                        Source("deps1_pkg1.ads"),
                    ],
                ),
                # Likewise here, even though it contains C units
                Project(
                    name="deps1_lib2",
                    sources=[
                        Source("deps1_extra2.ads"),
                        Source("deps1_pkg2.ads"),
                        Source("deps1_pkg3.c"),
                    ],
                ),
                # When they have no library interface, all Ada units in a
                # library project must be included.
                Project(
                    name="deps1_lib3",
                    sources=[
                        Source("deps1_pkg4.ads"),
                        Source("deps1_pkg5.ads"),
                    ],
                    is_library=True,
                ),
                # For library projects, it's the closure of the library
                # interface that matters:
                #
                # 1) dependents cannot depend on non-interface units,
                # 2) units in the library interface closure are instrumented
                #    even if they are not in the closure of a main.
                Project(
                    name="deps1_lib4",
                    sources=[
                        Source("deps1_extra3.ads"),
                        Source("deps1_pkg6.ads", deps=["deps1_pkg8"]),
                        Source("deps1_pkg7.ads"),
                        Source("deps1_pkg8.ads"),
                    ],
                    is_library=True,
                    library_interface_attr=(
                        "for Library_Interface use"
                        ' ("deps1_pkg6", "deps1_pkg7");'
                    ),
                ),
            ],
        ),
        expected_xcov={
            "deps1_main.adb",
            "deps1_pkg1.ads",
            "deps1_pkg2.ads",
            "deps1_pkg3.c",
            "deps1_pkg4.ads",
            "deps1_pkg5.ads",
            "deps1_pkg6.ads",
            "deps1_pkg7.ads",
            "deps1_pkg8.ads",
        },
        missing_unit_info={
            "deps1_extra1",
            "deps1_extra2",
            "deps1_extra3",
        },
    ),
    # Check with a C main: this time, all units in the dependencies
    # (deps1_lib1, deps1_lib2, lib1) should be instrumented, except in
    # dependencies that are library projects with a library interface (lib2).
    Test(
        root_project=Project(
            name="deps2_root",
            sources=[
                Source(
                    "deps2_main.c",
                    is_main=True,
                    c_deps=["deps1_pkg3.c"],
                ),
                Source(
                    "deps2_pkg1.ads",
                    deps=["lib1_pkg1", "lib2_pkg1"],
                ),
            ],
            deps=[
                "deps1_lib1",
                "deps1_lib2",
                "lib1",
                "lib2",
            ],
        ),
        expected_xcov={
            "deps1_extra1.ads",
            "deps1_extra2.ads",
            "deps1_pkg1.ads",
            "deps1_pkg2.ads",
            "deps1_pkg3.c",
            "deps2_main.c",
            "deps2_pkg1.ads",
            "lib1_pkg1.ads",
            "lib1_pkg2.ads",
            "lib2_pkg1.ads",
            "lib2_pkg3.ads",
        },
        missing_unit_info={"lib2_pkg2"},
    ),
    # Check with no main: all units from all dependencies should be
    # instrumented.
    Test(
        root_project=Project(
            name="deps3_root",
            sources=[Source("deps3_pkg1.ads", deps=["deps1_pkg1"])],
            deps=["deps1_lib1", "deps1_lib2"],
        )
    ),
]


def check_same_sets(label, expected, actual):
    """
    Make the test fail if ``expected`` and ``actual`` do not contain equivalent
    sets.
    """
    for status, items in [
        ("Missing", expected - actual),
        ("Spurious", actual - expected),
    ]:
        thistest.fail_if(
            items,
            f"{status} {label}:\n"
            + "\n".join(f"  {f}" for f in sorted(items)),
        )


for t in tests:
    # There is nothing to test in binary traces mode if we have no main in this
    # project tree.
    if thistest.options.trace_mode == "bin" and not t.root_project.mains:
        continue

    label = t.root_project.name
    thistest.log(f"== {label} ==")

    files_to_save = []

    # Compute the list of expected SID files from the list of ALI files created
    # by running gprbuild on the uninstrumented project.
    #
    # We just want ALI files to be created, so no need to do the link.
    #
    # Do it before "gnatcov instrument" so that gprbuild is not influenced by
    # instrumented sources.
    if thistest.options.trace_mode == "src":
        gprbuild(t.root_project.filename, gargs=["-c"])
        expected_sid_files = {
            # Consider all ALIs for "original units" (not the extra ones that
            # "gnatcov instrument" creates).
            f.replace(".ali", ".sid").replace("\\", "/")
            for f in glob.glob("obj/*/*.ali")
            if not any(s in f for s in ("/gcvrt-", "/gcvrt.", "/b__"))
        }

    if t.root_project.mains:
        # This project has mains: instrument/compile/run them and compute a
        # coverage report.
        xcov_dir = f"xcov-{label}"
        build_run_and_coverage(
            gprsw=GPRswitches(root_project=t.root_project.filename),
            covlevel="stmt",
            mains=[os.path.splitext(main)[0] for main in t.root_project.mains],
            extra_coverage_args=["-axcov", f"--output-dir={xcov_dir}"],
            gpr_obj_dir=t.root_project.objdir,
            tolerate_coverage_messages=(
                f"warning: no {info_file} file found for unit .*"
            ),
        )
        files_to_save.append("coverage.log")

        # Check we have the expected warnings about missing SIDs/ALIs
        thistest.fail_if_not_equal(
            "gnatcov output",
            "\n".join(
                f"warning: no {info_file} file found for unit {unit}"
                for unit in sorted(t.missing_unit_info)
            ),
            "\n".join(lines_of("coverage.log")),
        )

        # Check that we have the expected xcov reports
        check_same_sets(
            "XCOV reports",
            t.expected_xcov,
            {
                os.path.splitext(f)[0]
                for f in os.listdir(f"{xcov_dir}")
                if f.endswith(".xcov")
            },
        )
    elif thistest.options.trace_mode == "src":
        # This project has no main: just check that "gnatcov instrument"
        # produces the same set of SID files as the set of ALIs files generated
        # by gprbuild/gcc.
        xcov_instrument(
            gprsw=GPRswitches(root_project=t.root_project.filename),
            covlevel="stmt",
            gpr_obj_dir=t.root_project.objdir,
        )
        files_to_save.append("instrument.log")

    if thistest.options.trace_mode == "src":
        check_same_sets(
            "SID files",
            expected_sid_files,
            {
                # Consider all SIDs for Ada units (but not SIDs in the lib
                # directory, which are duplicates of the SIDs in the obj
                # directory).
                f.replace("\\", "/")
                for f in glob.glob("obj/*/*.sid")
                if not (f.endswith(".c.sid") or "-lib/" in f or "-lib\\" in f)
            },
        )

    # Rename the object directories so that 1) they are available for
    # post-mortem debugging 2) they do not interfere with the next test.
    save_dir = f"save-{label}"
    mv("obj", save_dir)
    for f in files_to_save:
        mv(f, save_dir)

thistest.result()
