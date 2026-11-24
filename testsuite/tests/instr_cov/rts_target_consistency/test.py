"""
Check target aliases and runtime mismatches when loading setup metadata.
"""

import json
from enum import Enum, auto
from pathlib import Path
import re

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")
install = Path("install").resolve()
runtime = "rts_consistency"
xcov(
    ["setup", f"--prefix={install}", f"--install-name={runtime}"],
    out="setup.log",
)
env.add_search_path("GPR_PROJECT_PATH", str(install / "share/gpr"))
metadata = install / f"share/gnatcov_rts/setup-config-{runtime}.json"
original = json.loads(metadata.read_text())

# Default runtimes can have an empty name, which disables runtime checks
project = gprfor(
    mains=["main.adb"],
    srcdirs=[".."],
    extra=f'for Runtime ("Ada") use "{original["runtime-dir"]}";',
)


class WarningKind(Enum):
    NONE = auto()
    TARGET = auto()
    RUNTIME = auto()


def check(
    label: str,
    changes: dict[str, str],
    warning: WarningKind = WarningKind.NONE,
    extra_args: list[str] | None = None,
) -> None:
    """Change only setup metadata, then check the consistency diagnostic."""
    thistest.log(f"== {label} ==")
    metadata.write_text(json.dumps(original | changes))
    output = f"{label}.log"
    kind = warning.name.lower()
    xcov_instrument(
        gprsw=GPRswitches(project),
        covlevel="stmt",
        runtime_project=runtime,
        extra_args=["-f", f'--target={original["target"]}']
        + (extra_args or []),
        gpr_obj_dir="obj",
        out=output,
        tolerate_messages=".*",
        # Exercise autoconfiguration with the explicit target and custom DB
        auto_config_args=False,
        auto_target_args=False,
    )
    actual = contents_of(output)
    if warning is WarningKind.NONE:
        thistest.fail_if_not_equal(label, "", actual)
    else:
        expected = (
            "unknown" if warning is WarningKind.TARGET else "no-such-runtime"
        )
        thistest.fail_if_no_match(
            label,
            re.compile(
                f"warning: Current {kind} is:\n  [^\n]+\n"
                f"which is inconsistent with the {kind} for [^\n]+: \n"
                f"  {re.escape(expected)}\n"
            ),
            actual,
        )


check("same-target", {})
if original["target"] == "x86_64-linux":
    check("target-alias", {"target": "x86_64-pc-linux-gnu"})
check("different-target", {"target": "no-such-target"}, WarningKind.TARGET)

# Resolve aliases using the project's custom target definitions
custom_db = Path("custom-db")
custom_db.mkdir()
(custom_db / "target.xml").write_text(
    '<?xml version="1.0"?>\n<gprconfig>\n'
    f'  <targetset canonical="{original["target"]}">\n'
    "    <target>alias-only-in-custom-db</target>\n"
    "  </targetset>\n</gprconfig>\n"
)
check(
    "custom-alias",
    {"target": "alias-only-in-custom-db"},
    extra_args=[f"--db={custom_db.resolve()}"],
)
check(
    "custom-different-target",
    {"target": "no-such-target"},
    WarningKind.TARGET,
    [f"--db={custom_db.resolve()}"],
)

# Equal targets must still trigger the runtime consistency check
check(
    "different-runtime",
    {"runtime-dir": "no-such-runtime", "runtime-name": "no-such-runtime"},
    WarningKind.RUNTIME,
)
thistest.result()
