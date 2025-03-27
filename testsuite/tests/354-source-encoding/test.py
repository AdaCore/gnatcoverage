"""Check that XML reports are valid for non-ASCII sources."""

import xml.dom.minidom as minidom

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov


def xml_src_summary(filename):
    """
    Read the given XML source file coverage report and extract a summary of
    the quoted lines it contains.
    """
    result = []

    def append(level, tag, content=None):
        prefix = "  " * level + tag
        result.append(
            f"{prefix}: {ascii(content)}" if content is not None else prefix
        )

    def element_nodes(parent):
        return [n for n in parent.childNodes if isinstance(n, minidom.Element)]

    def process_src_mapping(n):
        assert n.tagName == "src_mapping"
        append(0, "src_mapping")
        for c in element_nodes(n):
            match c.tagName:
                case "src":
                    process_src(c, 1)
                case "statement":
                    process_statement(c, 1)
                case "decision":
                    process_decision(c, 1)
                case "message":
                    pass
                case _:
                    raise AssertionError(
                        f"unexpected src_mapping child element: {c.tagName}"
                    )

    def process_src(n, level):
        assert n.tagName == "src"
        append(level, "src")
        for c in element_nodes(n):
            match c.tagName:
                case "line":
                    append(
                        level + 1,
                        f"line {c.getAttribute('num')}",
                        c.getAttribute("src"),
                    )
                case _:
                    raise AssertionError(
                        f"unexpected src child element: {c.tagName}"
                    )

    def process_statement(n, level):
        append(level, "statement", n.getAttribute("text"))
        for c in element_nodes(n):
            match c.tagName:
                case "src":
                    process_src(c, level + 1)
                case _:
                    raise AssertionError(
                        f"unexpected statement child element: {c.tagName}"
                    )

    def process_decision(n, level):
        append(level, "decision", n.getAttribute("text"))
        for c in element_nodes(n):
            match c.tagName:
                case "src":
                    process_src(c, level + 1)
                case "condition":
                    process_condition(c, level + 1)
                case _:
                    raise AssertionError(
                        f"unexpected decision child element: {c.tagName}"
                    )

    def process_condition(n, level):
        append(level, "condition", n.getAttribute("text"))
        for c in element_nodes(n):
            match c.tagName:
                case "src":
                    process_src(c, level + 1)
                case _:
                    raise AssertionError(
                        f"unexpected condition child element: {c.tagName}"
                    )

    doc = minidom.parse(filename)
    source = doc.documentElement
    for n in element_nodes(source):
        if n.tagName == "src_mapping":
            process_src_mapping(n)
    return "\n".join(result)


def check_xml_srclines(filename, expected_summary):
    """
    Read "filename" using xml_src_summary and check that it yields the given
    expected summary.
    """
    baseline = "tmp.txt"
    with open(baseline, "w") as f:
        f.write(expected_summary)
    thistest.fail_if_diff_internal(
        baseline,
        xml_src_summary(filename),
        failure_message=f"unexpected source excerpts in {filename}",
        ignore_white_chars=False,
    )


def create_summary(
    name,
    full_content,
    truncated_content,
):
    """
    Create a XML source file summary compatible with xml_src_summary for one of
    this testcase's source file.
    """

    # Adjust expectations for binary traces, which provide imprecise slocs
    line_6_cond = "A)" if thistest.options.trace_mode == "src" else "A"

    result = [
        "src_mapping",
        "  src",
        "    line 1: 'pragma Ada_2012;'",
        "src_mapping",
        "  src",
        f"    line 2: 'package {name} is'",
        "src_mapping",
        "  src",
        "    line 3: '   function Id (V : Boolean) return Boolean is (V);'",
        "  statement: 'V'",
        "    src",
        "      line 3: '                                                V'",
        "src_mapping",
        "  src",
        "    line 4: '   function F (A, B : Boolean) return Boolean'",
        "src_mapping",
        "  src",
        f"    line 5: {ascii(f'   is (Id (-- {full_content}')}",
        f"  statement: {ascii(f'Id (-- {truncated_content}...')}",
        "    src",
        f"      line 5: {ascii(f'       Id (-- {full_content}')}",
        "      line 6: '           A) and then B'",
        f"  decision: {ascii(f'Id (-- {truncated_content}...')}",
        "    src",
        f"      line 5: {ascii(f'       Id (-- {full_content}')}",
        "      line 6: '           A) and then B'",
        f"    condition: {ascii(f'Id (-- {truncated_content}...')}",
        "      src",
        f"        line 5: {ascii(f'       Id (-- {full_content}')}",
        f"        line 6: '           {line_6_cond}'",
        "    condition: 'B'",
        "      src",
        "        line 6: '                       B'",
        "src_mapping",
        "  src",
        "    line 6: '           A) and then B);'",
        "src_mapping",
        "  src",
        f"    line 7: 'end {name};'",
    ]
    return "\n".join(result)


tmp = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["--annotate=xml"],
)


def check_report(
    label: str,
    encoding: str | None,
    expected_warnings: list[str],
    latin1_summary: str,
    utf8_summary: str,
):
    """
    Check XML report production.

    :param label: Name for this check.
    :param encoding: Optional non-default encoding to pass to "gnatcov
        coverage".
    :param expected_warnings: Exhaustive list of warnings that are expected
        from "gnatcov coverage".
    :param latin1_summary: Expected summary of the XML report for latin1.ads.
    :param utf8_summary: Expected summary of the XML report for utf8.ads.
    """
    thistest.log(f"== {label} ==")

    log_file = f"coverage-{label}.log"

    # Make gnatcov produce the XML report for the given encoding
    argv = xcov_args + [f"--output-dir=out-{label}"]
    if encoding is not None:
        argv.append(f"--source-encoding={encoding}")

    tolerate_messages = (
        "|".join(expected_warnings) if expected_warnings else None
    )
    xcov(argv, out=log_file, tolerate_messages=tolerate_messages)

    # Check that we have exacly the expected warnings
    thistest.fail_if_not_equal(
        "'gnatcov coverage' output",
        "\n".join(expected_warnings),
        contents_of(log_file).strip(),
    )

    # Check the content of the XML report for both sources
    check_xml_srclines(f"out-{label}/latin1.ads.xml", latin1_summary)
    check_xml_srclines(f"out-{label}/utf8.ads.xml", utf8_summary)


# By default, all sources are interpreted as Latin-1. Note that truncation
# changes depending on the encoding as the same sequence of bytes may not yield
# the same number of codepoints.
check_report(
    label="default",
    encoding=None,
    expected_warnings=[],
    latin1_summary=create_summary("Latin1", "\xc9a\xc9", "\xc9a"),
    utf8_summary=create_summary("UTF8", "\xc3\x89a\xc3\x89", "\xc3\x89"),
)

# Check report production when the same set of sources is interpreted as UTF-8.
# Invalid UTF-8 byte sequences (in latin1.ads) are turned into U+FFFD
# codepoints.
check_report(
    label="utf-8",
    encoding="utf-8",
    expected_warnings=["warning: latin1.ads:5: cannot decode as utf-8"],
    latin1_summary=create_summary("Latin1", "\ufffda\ufffd", "\ufffda"),
    utf8_summary=create_summary("UTF8", "\xc9a\xc9", "\xc9a"),
)

# Check that we correctly reject invalid encodings
thistest.log("== invalid ==")
p = xcov(
    xcov_args + ["--source-encoding=invalid"],
    out="coverage-invalid.log",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0,
    "[invalid] failure expected, but 'gnatcov coverage' succeeded",
)
thistest.fail_if_no_match(
    "'gnatcov coverage' output",
    ".*gnatcov.*: unsupported encoding for sources: 'invalid'",
    contents_of("coverage-invalid.log").strip(),
)

thistest.result()
