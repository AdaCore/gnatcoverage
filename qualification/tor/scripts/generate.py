#!/usr/bin/env python

import os
import rest
import glob
import re

DOC_DIR = "source"
ROOT_DIR = "../../../testsuite/Qualif/Ada"


def to_title(str):
        """Given an entity name return a suitable string to be insterted
        in the documentation"""
        m = re.search(r'^[0-9]+_(.*)$', str)
        if m is not None:
            str = m.group(1)
        return str.replace('_', ' ')


def get_content(filename):
        """Return content of a file"""
        fd = open(filename, 'r')
        content = fd.read()
        fd.close()
        return content


class DocGenerator(object):

    def __init__(self, root_dir, doc_dir):
        self.root_dir = os.path.abspath(root_dir)
        self.doc_dir = os.path.abspath(doc_dir)
        self.resource_list = set([])

    def file2docfile(self, filename):
        """Return the associated filename for a given path"""
        docfile = os.path.relpath(filename, self.root_dir)
        # If we are at the root directory then return our documentation
        # entry point.
        if docfile == ".":
            return "index.rst"

        docfile = docfile.replace('/', '_').replace('\\', '_') + ".rst"
        return docfile

    def ref(self, name):
        """Transform a string into other string suitable to be used as index
        name"""
        result = os.path.relpath(name, self.root_dir)
        return result.replace('/', '_').replace('\\', '_').replace('.', '_')

    def parent_globbing(self, dir, pattern, include_start_dir=False):
        """Look for src/[pattern] files in dir and its parents directory
        up to document root directory"""
        head = os.path.relpath(dir, self.root_dir)
        tail = ''
        if not include_start_dir:
            head, tail = os.path.split(head)
        files = set([])
        while len(head) > 0:
            files |= set(glob.glob(os.path.join(self.root_dir, head, 'src',
                                                pattern)))
            head, tail = os.path.split(head)
        return files

    def find_with_clauses(self, dir, sourcefile):
        content = get_content(sourcefile)
        # Remove all comments
        content = '\n'.join([k for k in content.splitlines() \
                             if not re.match('\s*--', k)])

        # Find all the with clauses
        matches = re.findall(r'(?:\n|;|^)\s*with\s*([^;]+)\s*;', content, re.S)
        matches = [k.replace(' ', '') for k in matches]
        result = []
        for m in matches:
            result += m.lower().split(',')
        result = set(result)

        # Remove support package
        result -= set(['support'])

        file_list = set([])
        for item in result:
            spec = self.parent_globbing(dir, item + '.ads', True)
            if len(spec) > 1:
                print 'warning: find several spec for %s unit' % item
            file_list |= spec
            body = self.parent_globbing(dir, item + '.adb', True)
            if len(body) > 1:
                print 'warning: find several body for %s unit' % item
            elif len(body) == 1:
                file_list |= body
            if len(body | spec) == 0:
                print 'warning: no body or spec found for %s unit (%s)' % \
                      (item, sourcefile)
        return file_list

    def find_closure(self, dir, sourcefile):
        """Given an Ada source file find it's closure. Not that we ignore the
        support package"""

        result_set = self.find_with_clauses(dir, sourcefile)

        current_size = len(result_set)
        previous_size = 0
        while current_size > previous_size:

            previous_size = current_size
            tmp = set([])
            for item in result_set:
                tmp |= self.find_with_clauses(dir, item)

            result_set |= tmp
            current_size = len(result_set)

        return result_set

    def process_testcase(self, dir):
        # First find the Ada files in the current src subdirectory
        ada_files = set(glob.glob(os.path.join(dir, 'src', '*.ad[sb]')))

        # Find the first the tests procedures
        test_procedure_files = set([k for k in ada_files \
          if os.path.basename(k).startswith('test_')])

        if len(test_procedure_files) == 0:
            # We are in a case in which we don't have the procedure files in
            # the current directory. So the found them in the parent
            # directories using the names of the test data.
            data_names = set([os.path.basename(k).split('.')[0] \
                              for k in ada_files])
            for name in data_names:
                test_procedure_files |= \
                  self.parent_globbing(dir, 'test_' + name + '*.ad[sb]')

        # Find the closure of test procedures. This should give us the list of
        # data files
        test_data_files = set([])
        for d in test_procedure_files:
            test_data_files |= self.find_closure(dir, d)

        if len(test_data_files) == 0:
            print 'warning: no test data files'

        self.resource_list |= test_procedure_files | test_data_files
        return (test_procedure_files, test_data_files)

    def generate_doc(self, root_dir):
        """Generate documentation for root_dir and all its subdirectories"""
        for root, dirs, files in os.walk(os.path.abspath(root_dir)):

            dest_filename = self.file2docfile(root)
            dest_fd = open(os.path.join(self.doc_dir, dest_filename), 'w')
            name = os.path.basename(root)

            # Ignore some subdirectories
            for d in [k for k in dirs]:
                if d in ('.svn', 'src') or d.startswith('tmp_'):
                    dirs.remove(d)

            # Write the title of the section corresponding to the current dir
            dest_fd.write(rest.section(to_title(name)))

            # Sort subdirectories by alphanumerical order
            dirs.sort()

            # Check if we have a requirement
            if 'req.txt' in files:
                dest_fd.write("\n\n" + rest.strong("Requirement") + "\n\n")
                dest_fd.write(get_content(os.path.join(root, 'req.txt')))

                # Check if the requirement has some tests (check for tc.txt in
                # subdirs)
                dirs_copy = [k for k in dirs]
                test_id = 1
                has_testcase = False

                for d in dirs_copy:
                    if os.path.isfile(os.path.join(root, d, 'tc.txt')):
                        # There won't be any requirements in that directory
                        dirs.remove(d)
                        has_testcase = True

                        dest_fd.write("\n\n" + \
                          rest.strong("Test Case %d: %s" % \
                          (test_id, to_title(d))) + "\n\n")

                        dest_fd.write(get_content(os.path.join(root, d,
                                                               'tc.txt')))
                        test_id += 1

                        # Process the testcase
                        refs = self.process_testcase(os.path.join(root, d))

                        dest_fd.write('\n\n' + \
                                      rest.emphasis("Test Datas") + '\n\n')
                        dest_fd.write(rest.list([':ref:`%s`' % self.ref(d) \
                                                 for d in refs[1]]))
                        dest_fd.write('\n\n' + \
                                      rest.emphasis("Test Procedures") + \
                                      '\n\n')
                        dest_fd.write(rest.list([':ref:`%s`' % self.ref(d) \
                                                 for d in refs[0]]))
                if has_testcase and len(dirs) > 0:
                    for d in dirs:
                        print "warning: unexpected subreq or imcomplete " + \
                              "testcase in %s" % os.path.join(root, d)
                if not has_testcase and len(dirs) == 0:
                    print "warning: no testcase in leaf requirement %s" % root
            else:
                print "warning: invalid directory %s" % root

            if len(dirs) > 0:
                childs_list = [self.file2docfile(os.path.join(root, d)) \
                               for d in dirs]
                dest_fd.write(rest.toctree(childs_list))

            dest_fd.close()

    def generate_resources(self):
        fd = open(os.path.join(self.doc_dir, 'resources.rst'), 'w')
        fd.write(rest.section('Resources'))
        fd.write(rest.toctree([self.file2docfile(d) \
                 for d in self.resource_list]))
        fd.close()

        for r in self.resource_list:
            fd = open(os.path.join(self.doc_dir, self.file2docfile(r)), 'w')
            fd.write('\n.. _%s:\n\n' % self.ref(r))
            fd.write(rest.section(os.path.basename(r)))
            fd.write(rest.code_block(get_content(r), 'ada'))

    def generate_all(self):
        self.generate_doc(os.path.join(self.root_dir, 'stmt'))
        self.generate_doc(os.path.join(self.root_dir, 'decision'))
        self.generate_doc(os.path.join(self.root_dir, 'mcdc'))
        self.generate_resources()
        fd = open(os.path.join(self.doc_dir, 'index.rst'), 'w')
        fd.write(rest.chapter('Couverture'))
        chapter_files = [self.file2docfile(os.path.join(self.root_dir, d)) \
                         for d in ['stmt', 'mcdc']]
        fd.write(rest.toctree(chapter_files + ['resources.rst'], 8))
        fd.close()


# The main of the script
if __name__ == "__main__":
    mygen = DocGenerator(ROOT_DIR, DOC_DIR)
    mygen.generate_all()
