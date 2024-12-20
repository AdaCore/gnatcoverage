import qm
from qm.rest import ArtifactImporter, writer
from qm.rest.pdfgenerator import artifact_hash
from itertools import izip_longest
from collections import OrderedDict
import re
import os


MISSING_TR_LOG = os.path.join(qm.get_project_dir(), "missing_tr_log.txt")


def class_to_string(a):
    """
    Returns the two or three letters string
    refering to the class of the artifact 'a' if defined,
    the name of the class otherwise.

    :param a: the artifact
    :type a: artifact
    """
    d = {"TORReq_Set": "rqg", "TORReq": "rq", "TC": "tc", "TC_Set": "tcg"}

    if "Appendix" in a.full_name:
        return "app"
    elif a.name == "OpEnviron":
        return "env"
    elif a.__class__.__name__ in d:
        return d[a.__class__.__name__]
    else:
        return a.__class__.__name__


def class_to_content_key(a):
    """
    Returns key name for the main content of an artifact
    which is a container. (defined in xml model).

    :param a: the artifact
    :type a: artifact
    """

    # keys in the model are dependant of the artifact class
    d = {
        "TORReq_Set": "set_content",
        "TORReq": "requirement",
        "TC": "tc_content",
        "TC_Set": "tc_set_content",
    }

    if "Appendix" in a.full_name:
        return "app"
    elif a.name == "OpEnviron":
        # OpEnv only is content, not container
        return None
    elif a.__class__.__name__ in d:
        return d[a.__class__.__name__]
    else:
        return None


###########################
# Tests on artifact class #
###########################


def is_req(a):
    from qm import TORReq

    return isinstance(a, TORReq)


def is_reqset(a):
    from qm import TORReq_Set

    return isinstance(a, TORReq_Set)


def is_req_or_set(a):
    return is_req(a) or is_reqset(a)


def is_tc(a):
    from qm import TC

    return isinstance(a, TC)


def is_tcset(a):
    from qm import TC_Set

    return isinstance(a, TC_Set)


def is_tc_or_set(a):
    return is_tc(a) or is_tcset(a)


def is_test_result(a):
    from qm import TR

    return isinstance(a, TR)


def is_lrm_section(a):
    from qm import LRM_Section

    return isinstance(a, LRM_Section)


def is_source(a):
    from qm import Source_files

    return isinstance(a, Source_files)


def is_consolidation(a):
    from qm import Conso_Sources

    return isinstance(a, Conso_Sources)


def is_helper(source_resource):
    return not (is_driver(source_resource) or is_functional(source_resource))


def is_driver(source_resource):
    # a resource named with  "test_" is necsseraly a 'driver'
    return "test_" in source_resource.basename


def is_functional(source_resource):
    # a resource whom content contains "-- #" is
    # a functional source.  In case of Ada Sources !!

    content = source_resource.get_content()
    is_func = False

    for line in content.splitlines():
        if "-- #" in line:
            is_func = True
            break
    return is_func


#########
# Utils #
#########


def get_short_description(artifact):
    """
    Get the first line of a file as the short description.
    Layout elements are removed from the description.

    :param artifact: the artifact
    :type artifact: artifact
    """

    for item in artifact.contents(class_to_content_key(artifact)):
        content = item.get_content()

        for line in content.splitlines():
            line = line.strip()
            if len(line) > 0:
                #  ** has to be removed
                #  from short_description when used in tables
                first_line = line.replace("**", "")
                break

    return first_line


def get_first_req_relative(artifact):
    """
    Returns the first parent which is a req.

    :param artifact: the artifact
    :type artifact: artifact
    """
    parent = artifact.relative_to
    return (
        parent
        if (parent is None or is_req(parent))
        else get_first_req_relative(parent)
    )


def write_artifact_ref(artifact_full_name, label=None):
    """
    Returns a sphinx :ref: on 'artifact_full_name'
    using as label: 'label' if declared, 'artifact_full_name' otherwise

    :param artifact_full_name: the full name of the artifact
    :type artifact_full_name: string
    :param label: the label to be used un the hyperlink
    :type label: string
    """

    if label is None:
        label = artifact_full_name

    return writer.role(
        "ref", "%s <%s>" % (label, artifact_full_name.replace("/", "_")[1:])
    )


def kind_of(artifact):
    """
    :param artifact: the artifact
    :type artifact: artifact
    """
    return (
        "Requirement Group"
        if is_reqset(artifact)
        else (
            "Requirement"
            if is_req(artifact)
            else (
                "Testcase Group"
                if is_tcset(artifact)
                else "Testcase" if is_tc(artifact) else "Chapter"
            )
        )
    )


def short_kind_of(artifact):
    """
    :param artifact: the artifact
    :type artifact: artifact
    """
    kind = kind_of(artifact)
    return "Group" if "Group" in kind else kind


def relative_links_for(artifact):
    """
    :param artifact: the artifact
    :type artifact: artifact
    """
    output = ""

    req = get_first_req_relative(artifact)
    if req:
        output += writer.paragraph(
            "**Parent Requirement**: %s\n\n" % writer.qmref(req.full_name)
        )

    ancestor = artifact.relative_to
    if ancestor and ancestor != req:
        output += writer.paragraph(
            "**Parent %s**: %s\n\n"
            % (short_kind_of(ancestor), write_artifact_ref(ancestor.full_name))
        )

    return output


def default_importer(artifact):
    """
    Returns the importer whom the to_rest() method
    must be used for a given 'artifact' according to its class.

    :param artifact: the artifact
    :type artifact: artifact
    """
    if is_req(artifact):
        return RequirementImporter()
    elif is_tcset(artifact):
        return TCSetImporter()
    else:
        return qm.rest.DefaultImporter()


####################################################################
# Importers


class LRMTableImporter(ArtifactImporter):
    """
    The specific importer to generate the Traceability matrix
    between reference manual and testcases and requirements
    """

    def qmlink_to_rest(self, parent, artifacts):
        """
        Returns a matrix of traceability between
        lrm section artifact and thi associated test cases
        and requirements.
        chosen format was dicussed in MB23-001.

        :param parent: the artifact calling the qmlink
        :type parent: artifact
        :param artifacts: the list of artifacts listed in qmlink
        :type artifacts: list of artifacts
        """
        REQ_NAME_PREFIX = "/TOR/Ada"
        pdf_items = []
        html_items = []
        output = ""
        language_version = None

        for a in artifacts:
            if is_lrm_section(a):
                if not language_version:
                    language_version = a.attributes["language"].strip()

                ref = {}
                for children in a.derived_to:
                    for child in children.all:
                        if is_tc(child):
                            parent = get_first_req_relative(child).full_name

                            if parent not in ref.keys():
                                ref[parent] = []

                            ref[parent].append(
                                [
                                    child.full_name,
                                    child.full_name.replace(parent, ""),
                                ]
                            )

                pdf_tc_list = ""
                html_tc_list = ""
                pdf_comment = ""
                html_comment = ""

                for req in ref.keys():
                    pdf_other_tcs = ""
                    html_tcs = ""

                    if len(ref[req]) > 1:
                        pdf_other_tcs = "%s + %d other tests" % (
                            writer.role("raw-latex", r"\newline"),
                            (len(ref[req]) - 1),
                        )

                    html_tcs = writer.role("raw-html", r"<br>").join(
                        [
                            write_artifact_ref(k[0], label=k[1])
                            for k in ref[req]
                        ]
                    )

                    requirement_str = (
                        "Req: %s"
                        % write_artifact_ref(
                            req, req.replace(REQ_NAME_PREFIX, "")
                        ).strip()
                    )
                    first_tc_str = (
                        "* TC: %s"
                        % write_artifact_ref(
                            ref[req][0][0], label=ref[req][0][1]
                        ).strip()
                    )

                    pdf_tc_list += "%s %s %s %s %s " % (
                        requirement_str,
                        writer.role("raw-latex", r"\newline"),
                        first_tc_str,
                        pdf_other_tcs,
                        writer.role("raw-latex", r"\newline"),
                    )

                    html_tc_list += "%s %s  * TC: %s %s " % (
                        requirement_str,
                        writer.role("raw-html", r"<br>"),
                        html_tcs,
                        writer.role("raw-html", r"<br>"),
                    )

                applicable = a.attributes["relevance"].strip()
                if pdf_tc_list != "":
                    if applicable == "no" or applicable == "partial":
                        relevance = applicable
                        comment = a.attributes["comment"].strip()

                        pdf_comment = (
                            comment
                            + " "
                            + writer.role("raw-latex", r"\newline")
                            + " "
                        )
                        html_comment = (
                            comment
                            + " "
                            + writer.role("raw-html", r"<br>")
                            + " "
                        )

                    elif applicable == "yes":
                        relevance = "yes"
                    elif applicable == "no*":
                        relevance = "no"
                        comment = (
                            "Section does not require SCA-related "
                            + "tests, but some are supplied "
                        )

                        pdf_comment = (
                            comment
                            + writer.role("raw-latex", r"\newline")
                            + " "
                        )
                        html_comment = (
                            comment + writer.role("raw-html", r"<br>") + " "
                        )
                    else:
                        relevance = "unexpected value %s" % applicable

                    if relevance != "no":
                        # when applicable is set to no, the list of
                        # tc must be ommitted.
                        # otherwise it is added to the comment column
                        # see N102-011  Feb 8, 2014
                        pdf_comment += pdf_tc_list
                        html_comment += html_tc_list

                else:
                    if applicable == "no":
                        relevance = "no"
                        comment = a.attributes["comment"].strip()

                        pdf_comment = comment
                        html_comment = (
                            comment
                            + " "
                            + writer.role("raw-html", r"<br>")
                            + " "
                        )

                    elif applicable == "partial":
                        relevance = "PARTIAL but not covered"
                        comment = a.attributes["comment"].strip()

                        pdf_comment = comment
                        html_comment = (
                            comment
                            + " "
                            + writer.role("raw-html", r"<br>")
                            + " "
                        )

                    elif applicable == "yes":
                        relevance = "YES but not covered"
                    elif applicable == "no*":
                        relevance = "NO but"
                        comment = (
                            "Indicated as no* in matrix."
                            + " Some test should be provided."
                        )

                        pdf_comment = comment
                        html_comment = comment + " "

                    else:
                        relevance = "unexpected value %s" % applicable

                pdf_items.append(
                    [
                        "%s" % a.full_name.replace("/", ""),
                        a.attributes["title"].strip(),
                        relevance,
                        pdf_comment,
                    ]
                )

                html_items.append(
                    [
                        "%s" % a.full_name.replace("/", ""),
                        a.attributes["title"].strip(),
                        relevance,
                        html_comment,
                    ]
                )

        pdf_table = writer.csv_table(
            pdf_items,
            title="TOR/LRM Traceability Matrix for Ada %s" % language_version,
            headers=["Section", "Title", "Applicable", "Comment"],
            latex_format=r"|p{0.08\linewidth}|p{0.20\linewidth}|"
            + r"p{0.10\linewidth}|p{0.50\linewidth}|",
        )

        html_table = writer.csv_table(
            html_items,
            headers=["Section", "Title", "Applicable", "Comment"],
            widths=[8, 20, 10, 50],
        )

        output += (
            writer.paragraph(
                "This particular table is established for **Ada %s**."
                "\n\\The requirement identifiers in this table were shortened"
                " by removing the *%s* common prefix.\n\n"
                % (language_version, REQ_NAME_PREFIX)
            )
            + writer.only(pdf_table, "latex")
            + writer.only(html_table, "html")
        )

        output += "\n\n"

        return output, []


class TCIndexImporter(ArtifactImporter):
    def get_recursive_relatives(self, artifact, depth):
        """
        Returns the list of the tc or tc_set children of an artifact
        and recurcively the children of its children until the
        required depth

        :param artifact:
        :type artifact: artifact
        :param depth:
        :type depth: positive integer

        """
        result = []

        for child in artifact.relatives:
            if is_tc_or_set(child):
                result.append(child)
                if depth > 1:
                    result += self.get_recursive_relatives(child, depth - 1)

        return result

    def qmlink_to_rest(self, parent, artifacts):
        html_items = []
        pdf_items = []
        output = ""

        for a in artifacts:
            # Don't put sources in the tables
            if is_source(a):
                continue

            if is_tc_or_set(a):
                reference = write_artifact_ref(
                    a.full_name, get_short_description(a)
                )

            html_items.append(
                [
                    writer.strong(class_to_string(a)),
                    writer.strong(a.name),
                    reference,
                ]
            )
            pdf_items.append([class_to_string(a), a.name, reference])
            for suba in self.get_recursive_relatives(a, 1):
                # We do include in the table children artifacts only
                # in html format.

                if is_tc(suba):
                    subref = write_artifact_ref(
                        suba.full_name, get_short_description(suba)
                    )

                if is_tcset(suba):
                    subref = writer.qmref(suba.full_name)

                html_items.append(
                    [class_to_string(suba), "`..` %s" % suba.name, subref]
                )

        html_table = writer.csv_table(
            html_items,
            headers=["", "TestCases", "Description"],
            widths=[3, 25, 65],
        )

        pdf_table = writer.csv_table(
            pdf_items,
            headers=["", "TestCases", "Description"],
            widths=[3, 25, 65],
        ).strip()

        output += writer.only(html_table, "html")
        output += writer.only(pdf_table, "latex").strip()
        output += "\n\n"

        links = []
        for a in artifacts:
            if is_tc(a):
                links.append((a, TestCaseImporter()))
            elif is_source(a):
                pass
            else:
                links.append((a, default_importer(a)))

        output += writer.toctree(
            [
                "/%s/content" % artifact_hash(*link)
                for link in links
                if not is_tc_or_set(link[0]) or is_tc_or_set(parent)
            ],
            hidden=True,
        )

        return output, links


class AppIndexImporter(ArtifactImporter):
    def qmlink_to_rest(self, parent, artifacts):
        return "", []


class RequirementImporter(ArtifactImporter):
    """
    The specific importer for requirements
    """

    def to_rest(self, artifact):
        """
        Returns the 'rest' content of a requirement having the
        macro %REQ_ID% replaced by the requirement fullname
        """

        reference = ".. _%s:\n\n" % artifact.full_name.replace("/", "_")[1:]

        result = qm.rest.DefaultImporter().to_rest(artifact) + "\n\n"

        result = reference + re.sub(
            pattern="%REQ_ID%",
            repl="**REQUIREMENT** %s" % artifact.full_name,
            string=result,
        )

        return result


class TCSetImporter(ArtifactImporter):
    """
    The specific importer for TCSet
    """

    def to_rest(self, artifact):
        """
        Returns the 'rest' content of a tc set having the
        title and the links to parents included in the same 'minipage'
        in order to keep them close in the final pdf generation
        """

        reference = ".. _%s:\n\n" % artifact.full_name.replace("/", "_")[1:]
        result = ""
        qmlink = ""
        in_qmlink = False
        content = qm.rest.DefaultImporter().to_rest(artifact)

        for line in content.splitlines():
            if line.startswith(".. qmlink:: TCIndexImporter"):
                in_qmlink = True

            if in_qmlink:
                qmlink += line + "\n"
            else:
                result += line + "\n"

        result = reference + result + "\n\n"
        result += relative_links_for(artifact)
        result = (
            "|\n\n" + writer.minipage(result, r"\linewidth") + "\n\n" + qmlink
        )

        return result


class ToplevelIndexImporter(ArtifactImporter):
    def qmlink_to_rest(self, parent, artifacts):
        items = []
        html_top_index = ""

        for a in artifacts:
            items.append([writer.strong(a.name), writer.qmref(a.full_name)])

            if a.name == "Ada":

                def key(a):
                    d = {"stmt": 1, "decision": 2, "mcdc": 3}
                    for k in d:
                        if k in a.name:
                            return d[k]

                selected = [k for k in a.relatives if not is_source(k)]
                selected.sort(key=key)

            else:
                selected = a.relatives

            for suba in selected:
                items.append(
                    ["`..` %s" % suba.name, writer.qmref(suba.full_name)]
                )

        html_top_index += writer.csv_table(
            items, headers=["Chapter", "Description"], widths=[30, 70]
        )

        output = writer.only(html_top_index, "html")

        links = [
            (a, qm.rest.DefaultImporter())
            for a in artifacts
            if "Index/.+" not in a.full_name
        ]

        output += writer.toctree(
            [
                "/%s/content" % artifact_hash(*link)
                for link in links
                if not is_tc_or_set(link[0])
            ],
            hidden=True,
        )

        return output, links


class SubsetIndexTable(ArtifactImporter):
    def qmlink_to_rest(self, parent, artifacts):
        items = []
        header = ""

        req = len([a for a in artifacts if class_to_string(a) == "rq"])
        reqg = len([a for a in artifacts if class_to_string(a) == "rqg"])
        tcg = len([a for a in artifacts if class_to_string(a) == "tcg"])
        tc = len([a for a in artifacts if class_to_string(a) == "tc"])

        for a in artifacts:
            name = a.name
            items.append([class_to_string(a), name, writer.qmref(a.full_name)])

        # in the html, the title is adapted to the content of the table
        header = (
            "Requirements and Groups"
            if (req > 0 and reqg > 0)
            else (
                "Requirements Group"
                if (req == 0 and reqg == 1)
                else (
                    "Requirements Groups"
                    if (req == 0 and reqg > 1)
                    else (
                        "Requirement"
                        if (req == 1 and reqg == 0)
                        else (
                            "Requirements"
                            if (req > 1 and reqg == 0)
                            else (
                                "Testcases and Groups"
                                if (tc > 0 and tcg > 0)
                                else (
                                    "Testcases"
                                    if (tc > 0 and tcg == 0)
                                    else (
                                        "Testcases Groups"
                                        if (tc == 0 and tcg > 0)
                                        else ("")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

        output = writer.csv_table(
            items,
            headers=["", "%s" % header, "Description"],
            widths=[3, 25, 65],
        )

        return output, []


class SubsetIndexTocTree(ArtifactImporter):
    def qmlink_to_rest(self, parent, artifacts):
        links = [(a, default_importer(a)) for a in artifacts]

        output = writer.toctree(
            [
                "/%s/content" % artifact_hash(*link)
                for link in links
                if not is_tc_or_set(link[0])
            ],
            hidden=True,
        )

        return output, links


class SubsetIndexImporter(SubsetIndexTable):
    def qmlink_to_rest(self, parent, artifacts):
        output, links = SubsetIndexTable.qmlink_to_rest(
            self, parent, artifacts
        )

        links = [(a, default_importer(a)) for a in artifacts]

        output += writer.toctree(
            [
                "/%s/content" % artifact_hash(*link)
                for link in links
                if not is_tc_or_set(link[0])
            ],
            hidden=True,
        )
        return output, links


class TestCaseImporter(ArtifactImporter):
    def log_missing_TR(self, artifact):
        """
        Logs in a specific files the test case
        if it has no test results.

        :param artifact: the artifact to analyse
        :type artifact: artifact
        """

        has_TR = False

        for child in artifact.relatives:
            has_TR = is_test_result(child)
            if has_TR:
                break

        if not has_TR:
            with open(MISSING_TR_LOG, "a") as fd:
                fd.write(
                    "No TR for artifact %s   location:  %s\n"
                    % (artifact.full_name, artifact.location)
                )

    def get_sources(self, artifact):
        """
        Returns all the sources an artifact TC needs to
        list. It means its own sources and all thoses
        of its parents

        :param artifact: the artifact the sources are required from
        :type artifact: artifact
        """

        result = []

        for child in artifact.relatives:
            if is_source(child):
                result += [child]

        if artifact.relative_to is not None:
            result += self.get_sources(artifact.relative_to)

        return result

    def to_rest(self, artifact):
        reference = "\n\n.. _%s:\n" % artifact.full_name.replace("/", "_")[1:]

        result_pdf = "**TEST CASE**:  %s\n\n" % artifact.full_name
        result_html = "%s\n%s\n" % (
            artifact.full_name,
            "=" * len(artifact.full_name),
        )

        result_pdf += qm.rest.DefaultImporter().to_rest(artifact) + "\n\n"
        result_html += qm.rest.DefaultImporter().to_rest(artifact) + "\n\n"

        result = reference + writer.only(result_pdf, "latex")
        result += writer.only(result_html, "html")

        self.log_missing_TR(artifact)

        # Create a navigation links from this TC up to its requirement
        # and group if any:

        result += relative_links_for(artifact)

        # Managing the list of the sources

        driver_list = []
        driver_list_qmref = []
        func_list = []
        func_list_qmref = []
        helper_list = []
        helper_list_qmref = []

        consolidation_list = []
        consolidation_list_qmref = []

        # Whether we will output sources for the PDF version
        do_pdf = False

        for item in self.get_sources(artifact):
            if is_consolidation(item):
                consolidation_list += [item.name]
                consolidation_list_qmref += [
                    writer.qmref(item.full_name, item.name)
                ]

                continue

            for key in item.contents_keys:
                if len(item.contents(key)) > 0:
                    for resource in item.contents(key):
                        if is_driver(resource):
                            driver_list += [resource.basename]
                            driver_list_qmref += [
                                writer.qmref(item.full_name, resource.basename)
                            ]
                            continue

                        if is_functional(resource):
                            func_list += [resource.basename]
                            func_list_qmref += [
                                writer.qmref(item.full_name, resource.basename)
                            ]
                            continue

                        helper_list += [resource.basename]
                        helper_list_qmref += [
                            writer.qmref(item.full_name, resource.basename)
                        ]

        driver_list.sort()
        driver_list_qmref.sort()
        func_list.sort()
        func_list_qmref.sort()
        helper_list.sort()
        helper_list_qmref.sort()

        headers = ["Functional Sources", "Drivers Sources", "Helpers Sources"]

        if consolidation_list:
            headers += ["Consolidation Sources"]
            consolidation_list.sort()
            consolidation_list_qmref.sort()

            for_table_qmref = izip_longest(
                func_list_qmref,
                driver_list_qmref,
                helper_list_qmref,
                consolidation_list_qmref,
                fillvalue="",
            )

            for_table = izip_longest(
                func_list,
                driver_list,
                helper_list,
                consolidation_list,
                fillvalue="",
            )
        else:
            for_table_qmref = izip_longest(
                func_list_qmref,
                driver_list_qmref,
                helper_list_qmref,
                fillvalue="",
            )

            for_table = izip_longest(
                func_list, driver_list, helper_list, fillvalue=""
            )

        html_content = writer.csv_table(list(for_table_qmref), headers)

        result += writer.only(html_content, "html")

        if do_pdf:
            latex_content = writer.csv_table(list(for_table), headers).strip()
            result += writer.only(latex_content, "latex")

        output = "\n\n" + writer.minipage(result, r"\linewidth") + "|\n\n"

        return output


class SourceCodeImporter(ArtifactImporter):
    def to_rest(self, artifact):
        from qm import Ada_Sources, C_Sources, Conso_Sources

        result = ""

        if isinstance(artifact, Ada_Sources):
            for key in artifact.contents_keys:
                for item in artifact.contents(key):
                    result += writer.paragraph_title(item.basename)
                    result += writer.code_block(item.get_content(), "ada")

        if isinstance(artifact, C_Sources):
            for key in artifact.contents_keys:
                for item in artifact.contents(key):
                    result += writer.paragraph_title(item.basename)
                    result += writer.code_block(item.get_content(), "c")

        if isinstance(artifact, Conso_Sources):
            result += writer.paragraph_title(artifact.name)
            result += writer.code_block(
                artifact.location.get_content(), "bash"
            )

        return result


class IndexImporter(ArtifactImporter):
    def append_to_items(self, art, depth):
        if is_req(art):
            self.current_req = art
            kind_text = writer.strong("(%s)" % class_to_string(art))
            id_text = writer.strong("%s" % art.full_name.replace("/TOR", ""))

        elif is_tc(art):
            kind_text = "(%s)" % class_to_string(art)

            common_prefix_parent_req = os.path.commonprefix(
                (art.full_name, self.current_req.full_name)
            )

            id_text = "[...]" + art.full_name.replace(
                common_prefix_parent_req, ""
            )

        self.items.append(
            [
                kind_text,
                id_text,
                write_artifact_ref(art.full_name, get_short_description(art)),
            ]
        )

    def handle(self, art, depth):
        if is_req(art) or is_tc(art):
            self.append_to_items(art, depth)

        for child in art.relatives:
            self.handle(art=child, depth=depth + 1)

    def qmlink_to_rest(self, parent, artifacts):
        self.items = []

        def sortkey_for(art):
            # Arrange for stmt requirements to come first, before decision and
            # mcdc. Work from locations, which contain the explicit ordering
            # requests in the names (numeric prefixes like 1_).

            return str(art.location).replace("/stmt", "/a")

        artifacts.sort(key=sortkey_for)

        for art in artifacts:
            self.handle(art=art, depth=0)

        pdf_table = writer.csv_table(
            self.items,
            headers=["Kind", "Identification", "Description"],
            latex_format=(
                r"|p{0.05\linewidth}|p{0.47\linewidth}||p{0.37\linewidth}|"
            ),
        )

        html_table = writer.csv_table(
            self.items, headers=["Kind", "Ref"], widths=[5, 47, 37]
        )

        output = ""
        output += writer.only(pdf_table, "latex")
        output += writer.only(html_table, "html")

        output += "\n\n"

        return output, []


class TestCasesImporter(ArtifactImporter):
    def short_descs_of_main_ancestors(self, artifact, head):
        """
        Get the first line of both itself and the ancestor
        if the ancestor name is ended with 'head'

        :param artifact: the artifact
        :type artifact: artifact
        :param head: the prefix of the main group of artifacts
                     the description is required for
        :type head: string
        """

        parent = artifact.relative_to
        desc = None
        main_desc = None

        if parent is not None:
            if parent.full_name.endswith(head):
                main_desc = get_short_description(parent)
                desc = get_short_description(artifact)
            else:
                main_desc, desc = self.short_descs_of_main_ancestors(
                    parent, head
                )

        return main_desc, desc

    def get_testcases(self, artifact):
        """
        Returns either itself if artifact is a tc or a tc_set
        or the list of the test cases children of the artifact

        :param artifact: the artifact
        :type artifact: artifact
        """
        result = []

        if is_tc_or_set(artifact):
            return [artifact]

        for child in artifact.relatives:
            result += self.get_testcases(child)

        return result

    def get_sources(self, artifact):
        """
        Returns either itself if artifact is a source or
        the list of the sources children of artifact

        :param artifact: the artifact
        :type artifact: artifact
        """
        result = []

        if is_source(artifact):
            return [artifact]

        for child in artifact.relatives:
            result += self.get_sources(child)

        return result

    def tc_pdf_for(self, toplevel, subdirs, links):
        """
        String to emit in the PDF output for testcases (or sets)
        found in the ``links`` list underneath the list of candidate
        ``subdirs`` of the ``toplevel`` directory within the Qualif
        tree (eg. toplevel='Ada', subdirs=['stmt', 'decision', 'mcdc']).

        :param toplevel: Name of toplevel subdirectory within the
            Qualif hierarchy, such as 'Ada' or 'Common'.
        :type toplevel: string

        :param subdirs: List of immediate subdirectories of ``toplevel``
            where we should be searching for testcases, possiby through
            intermediate sets. There may be no testcase in any of these.
        :type subdirs: list[str]

        :param links:  List of (artifact, importer) tuples for actual
            tc_or_set artifacts to include in the document we're generating.
        :type links: list[(artifact,importer)]
        """

        pdf_output = ""

        for subdir in subdirs:
            subdir_output = self.tc_pdf_for_subdir(
                toplevel=toplevel, subdir=subdir, links=links
            )

            if subdir_output:
                if len(pdf_output) == 0:
                    pdf_output += (
                        writer.section("%s Testcases" % toplevel) + "\n"
                    )
                else:
                    pdf_output += (
                        writer.role("raw-latex", r"\newpage") + "\n\n"
                    )

                pdf_output += subdir_output

        return pdf_output

    def tc_pdf_for_subdir(self, toplevel, subdir, links):
        """
        Helper for tc_pdf_for, doing the work for a single subdir.
        """

        subdir_links = [
            sdl
            for sdl in links
            if sdl[0].full_name.startswith("/TOR/%s/%s" % (toplevel, subdir))
        ]

        if not subdir_links:
            return ""

        links_dict = OrderedDict()
        for sdl in subdir_links:
            main_desc, desc = self.short_descs_of_main_ancestors(
                sdl[0], subdir
            )
            if desc not in links_dict:
                links_dict[desc] = []
            links_dict[desc].append(sdl)

        pdf_output = ""
        pdf_output += writer.subsection("%s" % main_desc) + "\n"

        for desc in links_dict.keys():
            pdf_output += writer.subsubsection(desc) + "\n"
            pdf_output += writer.toctree(
                [
                    "/%s/content" % artifact_hash(*link)
                    for link in links_dict[desc]
                ],
                hidden=True,
            )

        return pdf_output

    def qmlink_to_rest(self, parent, artifacts):
        # cleanup missingTRfile
        with open(MISSING_TR_LOG, "w") as fd:
            fd.write("")

        # Precompute sets of (artifact, importer) pairs of relevance
        # to our outputs.

        tc_or_set_links = []
        src_links = []
        for a in artifacts:
            for suba in self.get_testcases(a) + self.get_sources(a):
                if is_tc(suba):
                    tc_or_set_links.append((suba, TestCaseImporter()))
                elif is_tcset(suba):
                    tc_or_set_links.append((suba, TCSetImporter()))
                elif is_source(suba):
                    src_links.append((suba, SourceCodeImporter()))

        mixed_links = src_links + tc_or_set_links

        # Build the html output

        html_output = writer.toctree(
            ["/%s/content" % artifact_hash(*link) for link in mixed_links],
            hidden=True,
        )

        # Then the PDF variant. A bit more work as we need to output
        # intermediate section titles ourselves and we don't want to
        # include the sources there.

        pdf_output = ""
        pdf_output += self.tc_pdf_for(
            toplevel="Ada",
            subdirs=["stmt", "decision", "mcdc"],
            links=tc_or_set_links,
        )
        pdf_output += self.tc_pdf_for(
            toplevel="Common",
            subdirs=["Report", "UnitsOfInterest", "GprFacilities"],
            links=tc_or_set_links,
        )

        output = writer.only(html_output, "html")
        output += writer.only(pdf_output, "latex")

        return output, mixed_links
