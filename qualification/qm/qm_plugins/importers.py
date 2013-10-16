import qm
from qm.rest import ArtifactImporter, writer
from qm.rest.pdfgenerator import artifact_hash
from itertools import izip_longest
from collections import OrderedDict
import re
import fileinput
import os
from os.path import join


MISSING_TR_LOG = os.path.join(qm.get_project_dir(), "missing_tr_log.txt")


def class_to_string(a):

    d = {'TORReq_Set': 'rqg',
         'TORReq': 'rq',
         'TC': 'tc',
         'TC_Set': 'tcg'}
    if 'Appendix' in a.full_name:
        return 'app'
    elif a.name == 'OpEnviron':
        return 'env'
    elif a.__class__.__name__ in d:
        return d[a.__class__.__name__]
    else:
        return a.__class__.__name__


def class_to_content_key(a):
    """
    Returns key name for content of various container
    """

    # keys in the model are dependant of the artifact class

    d = {'TORReq_Set': 'set_content',
         'TORReq': 'requirement',
         }

    if 'Appendix' in a.full_name:
        return 'app'
    elif a.name == 'OpEnviron':
        # OpEnv only is content, not container
        return None
    elif a.__class__.__name__ in d:
        return d[a.__class__.__name__]
    else:
        return None


##############################################################
# Tests on artifact class

def is_req_or_reqset(a):
    from qm import TORReq_Set, TORReq
    return isinstance(a, TORReq_Set) or isinstance(a, TORReq)


def is_req(a):
    from qm import TORReq
    return isinstance(a, TORReq)


def is_reqset(a):
    from qm import TORReq_Set
    return isinstance(a, TORReq_Set)


def is_test(a):
    from qm import TC, TC_Set
    return isinstance(a, TC) or isinstance(a, TC_Set)


def is_test_set(a):
    from qm import TC_Set
    return isinstance(a, TC_Set)


def is_test_case(a):
    from qm import TC
    return isinstance(a, TC)


def is_test_result(a):
    from qm import TR

    return isinstance(a, TR)


def is_source(a):
    from qm import Source_files
    return isinstance(a, Source_files)


def is_consolidation(a):
    from qm import Conso_Sources

    return isinstance(a, Conso_Sources)


def is_helper(source_resource):


    return not (is_test_driver(source_resource)
                or is_functional_source(source_resource))


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

############################################################


class TCIndexImporter(ArtifactImporter):

    def get_recursive_relatives(self, artifact, depth):
        result = []

        for child in artifact.relatives:
            if is_test(child):
                result.append(child)
                if depth > 1:
                    result += self.get_recursive_relatives(child, depth - 1)

        return result

    def qmlink_to_rest(self, parent, artifacts):
        from qm import TC
        html_items = []
        pdf_items = []
        output = ''

        for a in artifacts:
            # Don't put sources in the tables
            if is_source(a):
                continue

            html_items.append([writer.strong(class_to_string(a)),
                               writer.strong(a.name),
                               writer.qmref(a.full_name)])
            pdf_items.append([class_to_string(a),
                              a.name,
                              writer.qmref(a.full_name)])
            for suba in self.get_recursive_relatives(a, 1):
                # We do include in the table children artifacts only
                # in html format.
                html_items.append([class_to_string(suba),
                                   "`..` %s" % suba.name,
                                   writer.qmref(suba.full_name)])

        html_table = writer.csv_table(
            html_items,
            headers=["", "TestCases", "Description"],
            widths=[3, 25, 65])

        pdf_table = writer.csv_table(
            pdf_items,
            headers=["", "TestCases", "Description"],
            widths=[3, 25, 65])

        output += writer.only(html_table, "html")
        output += writer.only(pdf_table, "latex")

        links = []
        for a in artifacts:
            if is_test_case(a):
                links.append((a, TestCaseImporter()))
            elif is_source(a):
                pass
            else:
                links.append((a, qm.rest.DefaultImporter()))

        output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                  for l in links if
                                  not is_test(l[0]) or is_test(parent)],
                                 hidden=True)
        return output, links


class AppIndexImporter(ArtifactImporter):

    def qmlink_to_rest(self, parent, artifacts):
        return '', []


class ToplevelIndexImporter(ArtifactImporter):

    def qmlink_to_rest(self, parent, artifacts):

        items = []
        html_top_index = ""

        for a in artifacts:

            items.append([writer.strong(a.name),
                          writer.qmref(a.full_name)])

            if a.name == "Ada":

                def key(a):
                    d = {'stmt': 1, 'decision': 2, 'mcdc': 3}
                    for k in d:
                        if k in a.name:
                            return d[k]

                selected = [k for k in a.relatives if not is_source(k)]
                selected.sort(key=key)

            else:
                selected = a.relatives

            for suba in selected:
                items.append(["`..` %s" % suba.name,
                              writer.qmref(suba.full_name)])

        html_top_index += writer.csv_table(
            items,
            headers=["Chapter", "Description"],
            widths=[40, 60])

        output = writer.only(html_top_index, "html")

        links = [(a, qm.rest.DefaultImporter()) for a in artifacts]

        output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                  for l in links if not is_test(l[0])],
                                 hidden=True)

        return output, links


class SubsetIndexTable(ArtifactImporter):

    def qmlink_to_rest(self, parent, artifacts):

        items = []
        header = ""

        req = len([a for a in artifacts if class_to_string(a) == 'rq'])
        reqg = len([a for a in artifacts if class_to_string(a) == 'rqg'])

        for a in artifacts:

            name = a.name
            items.append([class_to_string(a),
                          name,
                          writer.qmref(a.full_name)])

        # in the html, the title is adapted to the content of the table
        header = ("Requirements and Groups" if (req > 0 and reqg > 0) else
                  ("Requirements Group" if (req == 0 and reqg == 1) else
                   ("Requirements Groups" if (req == 0 and reqg > 1) else
                    ("Requirement" if (req == 1 and reqg == 0) else
                     ("Requirements" if (req > 1 and reqg == 0) else
                      "")))))

        output = writer.csv_table(
            items,
            headers=["", "%s" % header, "Description"],
            widths=[3, 25, 65])

        return output, []


class SubsetIndexTocTree(ArtifactImporter):

    def qmlink_to_rest(self, parent, artifacts):

        links = [(a, qm.rest.DefaultImporter()) for a in artifacts]

        output = writer.toctree(['/%s/content' % artifact_hash(*l)
                                 for l in links if not is_test(l[0])],
                                hidden=True)

        return output, links


class SubsetIndexImporter(SubsetIndexTable):

    def qmlink_to_rest(self, parent, artifacts):

        output, links = SubsetIndexTable.qmlink_to_rest(self,
                                                        parent, artifacts)

        links = [(a, qm.rest.DefaultImporter()) for a in artifacts]

        output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                  for l in links if not is_test(l[0])],
                                 hidden=True)
        return output, links


class TestCaseImporter(ArtifactImporter):

    def get_first_req_relative(self, artifact):
        """
        Returns the first parent which is a req
        (and therefore the req that leads the TC)
        """

        parent = artifact.relative_to

        if parent is None or is_req_or_reqset(parent):
            return parent
        else:
            return self.get_first_req_relative(parent)

    def log_missing_TR(self, artifact):

        has_TR = False

        for child in artifact.relatives:
            has_TR = is_test_result(child)
            if has_TR:
                break

        if not has_TR:
            with open(MISSING_TR_LOG, 'a') as fd:
                fd.write("No TR for artifact %s   location:  %s\n" %
                         (artifact.full_name, artifact.location))

    def get_sources(self, artifact):
        """
        Returns all the sources an artifact TC needs to
        list. It means its own sources and all thoses
        of its parents
        """

        result = []

        for child in artifact.relatives:
            if is_source(child):
                result += [child]

        if artifact.relative_to is not None:
            result += self.get_sources(artifact.relative_to)

        return result

    def to_rest(self, artifact):

        result = ""

        result += qm.rest.DefaultImporter().to_rest(artifact) + '\n\n'

        self.log_missing_TR(artifact)

        req_parent = self.get_first_req_relative(artifact)

        if req_parent:
        # Creates a retro-link to navigate backward in the pdf
        # (from a TC to its requirement)
            result += writer.paragraph("Requirement :")
            result += writer.qmref(req_parent.full_name) + '\n\n'


        # Managing the list of the sources
        result += writer.directive('rubric', content=None, argument="Sources")

        driver_list = []
        driver_list_qmref = []
        func_list = []
        func_list_qmref = []
        helper_list = []
        helper_list_qmref = []

        consolidation_list = []
        consolidation_list_qmref = []

        for item in self.get_sources(artifact):

            if is_consolidation(item):

                consolidation_list += [item.name]
                consolidation_list_qmref += [writer.qmref
                                             (item.full_name,
                                              item.name)]

                continue

            for key in item.contents_keys:
                if len(item.contents(key)) > 0:

                    for resource in item.contents(key):

                        if is_driver(resource):
                            driver_list += [resource.basename]
                            driver_list_qmref += [writer.qmref
                                                  (item.full_name,
                                                   resource.basename)]
                            continue

                        if is_functional(resource):
                            func_list += [resource.basename]
                            func_list_qmref += [writer.qmref
                                                (item.full_name,
                                                 resource.basename)]
                            continue

                        helper_list += [resource.basename]
                        helper_list_qmref += [writer.qmref
                                              (item.full_name,
                                               resource.basename)]

        driver_list.sort()
        driver_list_qmref.sort()
        func_list.sort()
        func_list_qmref.sort()
        helper_list.sort()
        helper_list_qmref.sort()

        headers = ["Functional", "Drivers", "Helpers"]

        if consolidation_list:
            headers += ["Consolidation"]
            consolidation_list.sort()
            consolidation_list_qmref.sort()

            for_table_qmref = izip_longest(func_list_qmref,
                                           driver_list_qmref,
                                           helper_list_qmref,
                                           consolidation_list_qmref,
                                           fillvalue="")

            for_table = izip_longest(func_list,
                                     driver_list,
                                     helper_list,
                                     consolidation_list,
                                     fillvalue="")
        else:
            for_table_qmref = izip_longest(func_list_qmref,
                                           driver_list_qmref,
                                           helper_list_qmref,
                                           fillvalue="")

            for_table = izip_longest(func_list,
                                     driver_list,
                                     helper_list,
                                     fillvalue="")


        html_content = writer.csv_table([k for k in for_table_qmref],
                                        headers)

        latex_content = writer.csv_table([k for k in for_table],
                                         headers)

        result += writer.only(html_content, "html")

        result += writer.only(latex_content, "latex")

        return result


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
            result += writer.code_block(artifact.location.get_content(), "bash")

        return result


class TestCasesImporter(ArtifactImporter):

    def header_of_first_parent_after_head(self, artifact, head):
       """
       Get the first line of the 'last' parent before the 'head' requirement
       """

       parent = artifact.relative_to
       first_line = None

       if parent is not None:

          if parent.full_name.endswith(head):
               for item in artifact.contents(class_to_content_key(artifact)):
                   content = item.get_content()

                   for line in content.splitlines():
                       line = line.strip()
                       if len(line) > 0:
                           first_line = line
                           break
          else:
              first_line = self.header_of_first_parent_after_head(parent, head)

       return first_line


    def get_testcases(self, artifact):
        result = []

        if is_test(artifact):
            return [artifact]

        for child in artifact.relatives:
            result += self.get_testcases(child)

        return result

    def get_sources(self, artifact):
        result = []

        if is_source(artifact):
            return [artifact]

        for child in artifact.relatives:
            result += self.get_sources(child)

        return result

    def qmlink_to_rest(self, parent, artifacts):

        from qm import Source_files, TC, TC_Set

        items = []
        for a in artifacts:
            items += self.get_testcases(a)
            items += self.get_sources(a)

        links = []
        for a in items:
            if is_test_case(a):
                links.append((a, TestCaseImporter()))
            elif is_source(a):
                links.append((a, SourceCodeImporter()))
            else:
                links.append((a, qm.rest.DefaultImporter()))

        html_output = writer.toctree(['/%s/content' % artifact_hash(*l)
                                      for l in links], hidden=True)

        # We don't include the tests sources in the pdf version
        pdf_output = writer.section('Ada Test Cases') + '\n'

        # cleanup missingTRfile
        with open(MISSING_TR_LOG, 'w') as fd:
            fd.write("")

        # stmt
        links_stmt = [l for l in links
                      if not is_source(l[0]) and "stmt" in l[0].full_name]

        if links_stmt:

            links_dict = OrderedDict()

            for l in links_stmt:
                first_line = self.header_of_first_parent_after_head(l[0], "stmt")

                if first_line not in links_dict:
                    links_dict[first_line] = []

                links_dict[first_line].append(l)

            pdf_output += writer.subsection('Statement Coverage (SC) \
                                            assesments') + '\n'

            for first_line in links_dict.keys():

                pdf_output += writer.subsubsection(first_line) + '\n'

                pdf_output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                             for l in links_dict[first_line]],
                                             hidden=True)

        # decision
        links_dec = [l for l in links
                     if not is_source(l[0]) and "decision" in l[0].full_name]

        if links_dec:

            links_dict = OrderedDict()

            for l in links_dec:
                first_line = self.header_of_first_parent_after_head(l[0], "decision")

                if first_line not in links_dict:
                    links_dict[first_line] = []

                links_dict[first_line].append(l)


            pdf_output += writer.raw ("\\newpage", "latex")
            pdf_output += writer.subsection('Decision Coverage (DC) \
                                            assesments') + '\n'

            for first_line in links_dict.keys():

                pdf_output += writer.subsubsection(first_line) + '\n'

                pdf_output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                             for l in links_dict[first_line]],
                                             hidden=True)


        links_mcdc = [l for l in links
                      if not is_source(l[0]) and "mcdc" in l[0].full_name]

        if links_mcdc:

            links_dict = OrderedDict()

            for l in links_mcdc:
                first_line = self.header_of_first_parent_after_head(l[0], "mcdc")

                if first_line not in links_dict:
                    links_dict[first_line] = []

                links_dict[first_line].append(l)

            pdf_output += writer.raw ("\\newpage", "latex")
            pdf_output += writer.subsection('Modified Condition / \
                                            Decision Coverage (MCDC) \
                                            assesments') + '\n'

            for first_line in links_dict.keys():

                pdf_output += writer.subsubsection(first_line) + '\n'

                pdf_output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                             for l in links_dict[first_line]],
                                             hidden=True)

        links_rep = [l for l in links
                     if not is_source(l[0]) and "Report" in l[0].full_name]

        if links_rep:
            pdf_output += writer.section('Language-independent \
                                            Test Cases') + '\n'
            pdf_output += writer.toctree(['/%s/content' % artifact_hash(*l)
                                         for l in links_rep],
                                         hidden=True)

        output = writer.only(html_output, "html")
        output += writer.only(pdf_output, "latex")

        return output, links
