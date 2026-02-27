# This modules exposes common utilty functions for integrated instrumentation
# tests.

from __future__ import annotations

import abc
from collections.abc import Iterable, Sequence
import dataclasses
import os
from pathlib import Path

from e3.os.process import Run

import SUITE.control
from SUITE.context import get_c_bsp_info, ROOT_DIR
from SUITE.cutils import contents_of, no_ext, text_to_file, to_list
from SUITE.tutils import cmdrun, run_cov_program, thistest, xcov


def find_files_with_exts(
    exts: Iterable[str],
    root_dir: str | None = None,
) -> list[str]:
    """
    Recursively find source files with the given extensions, starting at
    root_dir. If root_dir is None, start at the current directory.
    """
    root_dir = root_dir or os.getcwd()
    root_path = Path(os.getcwd())
    return [str(p) for p in root_path.rglob("*") if p.suffix in exts]


def driver_for_lang(lang: str) -> str:
    """
    Refer to SUITE.control.driver_for_lang.
    """
    result = SUITE.control.driver_for_lang(
        os.path.join(ROOT_DIR, "suite.cgpr"), lang
    )
    assert result
    return result


@dataclasses.dataclass(kw_only=True)
class Workflow:
    """
    Interface class for building workflows.
    """

    # Language to infer the compiler driver to use for compiling / linking
    lang: str = "C"

    # Where to setup and run the workflow
    cwd: str = "."

    # File containing the output of the build
    out_file = "build.out"

    def __post_init__(self) -> None:
        self.cwd = os.path.abspath(self.cwd)

    @abc.abstractmethod
    def build(
        self,
        env: dict[str, str],
        register_failure: bool = True,
    ) -> Run:
        pass


@dataclasses.dataclass(kw_only=True)
class LinkBits(Workflow):
    """
    Shared common code between all of the workflows linking an executable.
    """

    # Switches to pass when linking
    linker_switches: list[str] = dataclasses.field(default_factory=list)

    # Name for the executable
    exec_name: str = "main"

    def __post_init__(self) -> None:
        super().__post_init__()
        self.linker_switches = to_list(self.linker_switches)
        if thistest.env.is_cross:
            bsp_info = get_c_bsp_info()
            assert bsp_info
            self.linker_switches += bsp_info.linker_switches

    def linker(self, lang: str) -> str:
        return os.path.basename(driver_for_lang(lang))

    def get_executable(self) -> str:
        return os.path.join(self.cwd, self.exec_name)


@dataclasses.dataclass(kw_only=True)
class CompileBits(Workflow):
    """
    Shared common code between all of the workflows compiling code.
    """

    # Switches to pass when compiling
    compiler_switches: list[str] = dataclasses.field(default_factory=list)

    def __post_init__(self) -> None:
        super().__post_init__()
        if thistest.env.is_cross:
            bsp_info = get_c_bsp_info()
            assert bsp_info
            self.compiler_switches += bsp_info.compiler_switches

    def compiler(self, lang: str) -> str:
        return os.path.basename(driver_for_lang(lang))


@dataclasses.dataclass(kw_only=True)
class MakefileCommon(CompileBits):
    build_target: str = "None"

    # Template for the Makefile. Class deriving from the MakefileCommon class
    # must implement one.
    tmplt: str = "unimplemented"

    # Target dependencies for the build target
    build_target_deps: list[str]

    def template_params(self) -> dict[str, str]:
        return {
            "build_target": self.build_target,
            "build_target_deps": " ".join(self.build_target_deps),
            "cc": self.compiler("C"),
            "cxx": self.compiler("C++"),
            "compiler_switches": " ".join(self.compiler_switches),
        }

    def __post_init__(self) -> None:
        super().__post_init__()
        self.build_target_deps = to_list(self.build_target_deps)
        self.out_file = "make.out"

        template = contents_of(os.path.join(ROOT_DIR, "templates", self.tmplt))
        makefile_text = template.format(**self.template_params())
        text_to_file(
            text=makefile_text,
            filename=os.path.join(self.cwd, "Makefile"),
        )

    def build(
        self,
        env: dict[str, str],
        register_failure: bool = True,
    ) -> Run:
        return cmdrun(
            ["make", "-C", self.cwd, self.build_target],
            out=self.out_file,
            for_pgm=False,
            env=env,
            register_failure=register_failure,
        )

    def clean(self) -> None:
        cmdrun(
            ["make", "-C", self.cwd, "clean"],
            out="make.out",
            for_pgm=False,
        )


@dataclasses.dataclass(kw_only=True)
class MakefileMain(MakefileCommon, LinkBits):

    def template_params(self) -> dict[str, str]:
        return {
            **super().template_params(),
            "linker": self.linker(self.lang),
            "linker_switches": " ".join(self.linker_switches),
        }

    def __post_init__(self) -> None:
        # Initialize the template file prior to calling the super __post_init__
        # method that fills it.
        self.tmplt = "template.Makefile"
        self.build_target = self.exec_name
        super().__post_init__()


@dataclasses.dataclass(kw_only=True)
class MakefileSharedLib(MakefileCommon, LinkBits):

    def template_params(self) -> dict[str, str]:
        return {
            **super().template_params(),
            "linker": self.linker(self.lang),
            "linker_switches": " ".join(self.linker_switches),
        }

    def __post_init__(self) -> None:
        self.tmplt = "template.Makefile_shared_lib"
        if thistest.env.target.os.name == "windows":
            shared_lib_ext = ".dll"
        else:
            shared_lib_ext = ".so"
        if self.build_target == "None":
            self.build_target = "liblib" + shared_lib_ext
        super().__post_init__()


@dataclasses.dataclass(kw_only=True)
class MakefileStaticLib(MakefileCommon):

    def __post_init__(self) -> None:
        self.tmplt = "template.Makefile_static_lib"
        if self.build_target == "None":
            self.build_target = "liblib.a"
        super().__post_init__()


@dataclasses.dataclass(kw_only=True)
class CompileSource(CompileBits):
    out: str = ""
    source: str

    def __post_init__(self) -> None:
        super().__post_init__()
        if not self.out:
            self.out = no_ext(os.path.basename(self.source)) + ".o"
        self.out_file = "compile_" + os.path.basename(self.source) + ".out"

    def build(
        self,
        env: dict[str, str],
        register_failure: bool = True,
    ) -> Run:
        return cmdrun(
            [self.compiler(self.lang), self.source, "-c", "-o", self.out]
            + self.compiler_switches,
            for_pgm=False,
            out=self.out_file,
            env=env,
            register_failure=register_failure,
        )


@dataclasses.dataclass(kw_only=True)
class LinkMain(LinkBits):
    objects: list[str]

    def __post_init__(self) -> None:
        super().__post_init__()
        self.out_file = "link_" + self.exec_name + ".out"

    def build(
        self,
        env: dict[str, str],
        register_failure: bool = True,
    ) -> Run:
        return cmdrun(
            [self.linker(self.lang), "-o", self.exec_name]
            + self.objects
            + self.linker_switches,
            for_pgm=False,
            env=env,
            register_failure=register_failure,
        )


@dataclasses.dataclass(kw_only=True)
class RunCompiler(Workflow):
    switches: list[str]

    def __post_init__(self) -> None:
        super().__post_init__()
        self.compiler = os.path.basename(driver_for_lang(self.lang))
        self.out_file = f"{self.compiler}.out"

    def build(
        self,
        env: dict[str, str],
        register_failure: bool = True,
    ) -> Run:
        return cmdrun(
            [self.compiler] + self.switches,
            for_pgm=False,
            env=env,
            register_failure=register_failure,
        )


def setup_integration(
    files_of_interest: Iterable[str] | None = None,
    covlevel: str = "stmt",
    output_dir: str | None = None,
    extra_args: Iterable[str] | None = None,
) -> dict[str, str]:
    """
    Run the gnatcov setup-integration command.

    :param files_of_interest: Files of interest for coverage analysis. if None,
        defaults to all of the files with a C/C++ extension recursively found
        under the current directory.
    :param covlevel: Coverage level passed to the setup-integration command.
    :param output_dir: Output directory for gnatcov artifacts, current
        directory if None.
    :param extra_args: Arguments to forward to the setup-integration command.

    :return: Environment with the compiler wrapper on the PATH.
    """

    if not output_dir:
        output_dir = os.getcwd()

    # Find source of interests if not explicitly passed by the user
    if not files_of_interest:
        files_of_interest = find_files_with_exts([".c", ".cpp", ".h", ".hpp"])

    # Grab the compilers for C and C++
    compilers = [
        os.path.basename(driver_for_lang("C")),
        os.path.basename(driver_for_lang("C++")),
    ]

    # When running in a cross configuration, we need to use the coverage
    # runtime that is installed along the bsp support.
    if thistest.env.is_cross:
        gnatcov_rts = thistest.options.c_bsp_gnatcov_rts
    else:
        gnatcov_rts = "gnatcov_rts"

    # Setup the integration instrumentation
    xcov(
        [
            "setup-integration",
            f"--level={covlevel}",
            f"--output-dir={output_dir}",
            f"--runtime-project={gnatcov_rts}",
        ]
        + [f"--compilers={comp}" for comp in compilers]
        + [f"--files={f}" for f in files_of_interest]
        + to_list(extra_args or [])
    )
    env = dict(os.environ)
    env["PATH"] = "{}{}{}".format(output_dir, os.path.pathsep, env["PATH"])
    return env


def build_run_and_coverage(
    wfs: Sequence[Workflow],
    covlevel: str = "stmt",
    files_of_interest: Iterable[str] | None = None,
    extra_setup_args: Iterable[str] | None = None,
    register_failure: bool = True,
) -> None:
    """
    Integrated instrumentation workflow, using the given workflows to build the
    instrumented main.

    Run the setup-integrated gnatcov command to setup the integrated
    instrumentation process.

    Then, build the main with the provided workflows (derived from the Workflow
    class). The last provided workflow shall builds an executable whose name
    shall be provided by a get_executable method. See the LinkBits class above.

    Third, run the main, getting its name through the get_executable method of
    the last workflow, as mentionned before.

    Last, run the "gnatcov coverage" command, generating an xcov coverage
    report by default.

    :param wf: Workflows to build the main.
    :param covlevel: Coverage level.
    :param files_of_interest: List of files of interest. By default, consider
        recursively all the files with the following extension: .c, .h, .cpp,
        .hpp.
    :param extra setup_args: List of arguments to pass to the setup-integration
        command.
    :param register_failure: Exit on program failure.
    """

    output_dir = os.getcwd()

    # Setup the integrated instrumentation
    env = setup_integration(
        files_of_interest=files_of_interest,
        covlevel=covlevel,
        output_dir=output_dir,
        extra_args=extra_setup_args or [],
    )

    # Run the build processes with the modified environment
    for wf in wfs:
        p = wf.build(env, register_failure=register_failure)

        # Exit early if the build process failed
        if p.status != 0:
            return

    # Run the executable
    run_log = "run.log"
    last_wf = wfs[-1]
    assert isinstance(last_wf, LinkBits)
    p = run_cov_program(
        last_wf.get_executable(),
        out=run_log,
        register_failure=register_failure,
    )

    # If the program execution failed, it likely did not produce a source
    # trace. Do not compute coverage data in this case.
    if p.status != 0:
        return

    # Extract the trace from the program output if running in a cross
    # configuration.
    if thistest.env.is_cross:
        xcov(["extract-base64-trace", run_log, "main.srctrace"])

    # Then run the gnatcov coverage command
    xcov(
        [
            "coverage",
            f"--level={covlevel}",
            "-axcov",
        ]
        + find_files_with_exts([".srctrace"])
        + [
            f"--sid={sid}"
            for sid in find_files_with_exts([".sid"], output_dir)
        ]
    )
