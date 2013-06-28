import qm
from qm.rest import ArtifactImporter
from qm.rest.writer import csv_table, toctree, strong, generic_list, subsection
from qm.rest.pdfgenerator import artifact_hash
import os
import re


def class_to_string(a):
    d = {'Req_Set': 'rqg',
         'Req': 'rq',
         'TC': 'tc',
         'TC_Set': 'tcg'}
    if 'Appendix' in a.full_name:
        return 'app'
    elif a.name == 'OpEnviron':
        return 'intro'
    elif a.__class__.__name__ in d:
        return d[a.__class__.__name__]
    else:
        return a.__class__.__name__


def is_test(a):
    return a.__class__.__name__ in ("TC", "TC_Set")


class TCIndexImporter(ArtifactImporter):

    def get_recursive_relatives(self, artifact):
        result = []

        for child in artifact.relatives:
            result.append(child)
            result += self.get_recursive_relatives(child)

        return result

    def qmlink_to_rest(self, parent, artifacts):
        items = []
        for a in artifacts:
            items.append([strong(class_to_string(a)),
                          strong(a.name),
                          ":qmref:`%s`" % a.full_name])
            for suba in self.get_recursive_relatives(a):
                items.append([class_to_string(suba),
                              "``..`` " + suba.name,
                              ":qmref:`%s`" % suba.full_name])

        output = csv_table(
            items,
            headers=["(*)", "Chapter", "Description"],
            widths=[2, 20, 70])

        links = []
        for a in artifacts:
            if a.__class__.__name__ == 'TC':
                links.append((a, TestCaseImporter()))
            else:
                links.append((a, qm.rest.DefaultImporter()))

        output += toctree(['/%s/content' % artifact_hash(*l)
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
        for a in artifacts:
            items.append([strong(class_to_string(a)),
                          strong(a.name),
                          ":qmref:`%s`" % a.full_name])
            for suba in a.relatives:
                items.append([class_to_string(suba),
                              "``..`` " + suba.name,
                              ":qmref:`%s`" % suba.full_name])

        output = csv_table(
            items,
            headers=["(*)", "Chapter", "Description"],
            widths=[2, 20, 70])

        links = [(a, qm.rest.DefaultImporter()) for a in artifacts]

        output += toctree(['/%s/content' % artifact_hash(*l)
                           for l in links if not is_test(l[0])], hidden=True)
        return output, links


class SubsetIndexImporter(ArtifactImporter):

    def qmlink_to_rest(self, parent, artifacts):

        items = []
        for a in artifacts:
            name = re.sub(r'[0-9]*_(.*)', r'\1', a.name)
            items.append([class_to_string(a),
                          name,
                          ":qmref:`%s`" % a.full_name])

        output = csv_table(
            items,
            headers=["", "Requirement Group", "Description"])

        links = [(a, qm.rest.DefaultImporter()) for a in artifacts]

        output += toctree(['/%s/content' % artifact_hash(*l)
                           for l in links if not is_test(l[0])], hidden=True)

        return output, links


class TestCaseImporter(ArtifactImporter):
    def to_rest(self, artifact):

        result = qm.rest.DefaultImporter().to_rest(artifact) + '\n\n'

        for key in ("ada_sources", "c_sources", "h_sources"):

            if len(artifact.contents(key)) > 0:
                result += subsection(key.replace('_', ' '))
                result += generic_list([k.basename for k in
                                        artifact.contents(key)])

        return result


class TestCasesImporter(ArtifactImporter):

    def get_testcases(self, artifact):
        result = []

        if is_test(artifact):
            return [artifact]

        for child in artifact.relatives:
            result += self.get_testcases(child)

        return result

    def qmlink_to_rest(self, parent, artifacts):
        items = []
        for a in artifacts:
            items += self.get_testcases(a)

        links = []
        for a in items:
            if a.__class__.__name__ == 'TC':
                links.append((a, TestCaseImporter()))
            else:
                links.append((a, qm.rest.DefaultImporter()))

        output = toctree(['/%s/content' % artifact_hash(*l)
                          for l in links], hidden=True)

        return output, links
