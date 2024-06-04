"""
Perform a bunch of xcov related operations to validate the basic toolset
functionalities.
"""

import os

from SUITE.context import thistest
from SUITE.cutils import match
from SUITE.tutils import maybe_valgrind, xcov, xrun


# Built with:
# powerpc-elf-gcc -g -c d1/func.c -o d1/func.o
# powerpc-elf-gcc -g -c d2/func.c -o d2/func.o
# powerpc-elf-gcc -g -c main.c
# gprbuild --target=powerpc-elf --RTS=zfp-mpc8641 -g repro -largs main.o \
#    */func.o
# mv repro main.elf

# Context information, basic command line interface checks
print("maybe_valgrind prepends ...", maybe_valgrind([]))

xrun("main.elf")

formats = [
    ("xml", "{slug}.xml", r'<instruction_set coverage="\+">'),
    ("html", "{slug}.hunk.js", r'"coverage":"\+",.*"assembly"'),
    ("xcov+", "{slug}.xcov", r" *[0-9]+ \+:"),
]


# The pre-built binary embeds full paths to the sources at their build
# time location. We need to rebase to find the files in the current
# dir. The paths gnatcov uses are internally canonicalized, which yields
# Windows or Unix paths depending on the host ...


def hostify(path):
    return (
        "C:" + path.replace("/", "\\")
        if thistest.env.build.os.name == "windows"
        else path
    )


rebase_args = [
    "--source-rebase={}={}".format(
        hostify(f"{path}/tests/IA22-004-same-basename"), os.getcwd()
    )
    for path in (
        "/home/pmderodat/_git/gnatcoverage/testsuite",
        "/home/pmderodat/_git/gnatcoverage-extra",
    )
]

for fmt, filename, pattern in formats:
    xcov(
        [
            "coverage",
            "--level=insn",
            "--annotate=" + fmt,
            "main.elf.trace",
            "--output-dir=tmp",
        ]
        + rebase_args
    )

    for slug in ("d1-func.c", "d2-func.c"):
        output = "tmp/" + filename.format(base="func.c", slug=slug)
        thistest.fail_if(
            not match(pattern, output), "no line covered in {}".format(output)
        )
thistest.result()
