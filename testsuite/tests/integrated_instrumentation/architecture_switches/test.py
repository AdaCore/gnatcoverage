"""
Regression test: test that gnatcov correctly compile instrumentation artifacts
with the architecture-specific switches (such as -m32) passed to the original
compiler driver invocation. It used not to, which resulted in an error when
linking the instrumentation artifacts with the instrumented source, which was
compiled with them.
"""

from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import CompileSource, setup_integration
from SUITE.tutils import thistest

Wdir("tmp_")

# Try to compile the source: the test used to fail there, because gnatcov used
# to run partial linking ("gcc -r"), to combine the actual code unit
# (expectedly built with -m32) with the coverage buffer unit (unexpectedly
# built with default settings: -m64), which the linker rejected.

comp_wf = CompileSource(source="../pkg.c", compiler_switches="-m32")
env = setup_integration(files_of_interest=["../pkg.c"])
comp_wf.build(env=env)

thistest.result()
