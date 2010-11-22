#!/usr/bin/env python

import os
import rest
import glob

DOC_DIR = "source"
ROOT_DIR = "../../../testsuite/Qualif/Ada"


def to_title(str):
        """Given an entity name return a suitable string to be insterted
        in the documentation"""
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
        self.doc_dir  = os.path.abspath(doc_dir)
        self.resource_list = []

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

    def generate_doc(self, root_dir):
        """Generate documentation for root_dir and all its subdirectories"""
        for root, dirs, files in os.walk(os.path.abspath(root_dir)):

            dest_filename = self.file2docfile(root)
            dest_fd = open(os.path.join(self.doc_dir, dest_filename), 'w')
            print "+ Generating %s" % dest_filename
            name = os.path.basename(root)

            # Ignore some subdirectories
            if '.svn' in dirs:
                dirs.remove('.svn')
            if 'src' in dirs:
                dirs.remove('src')

            # Write the title of the section corresponding to the current dir
            dest_fd.write(rest.section(to_title(name)))

            # Sort subdirectories by alphanumerical order
            dirs.sort()

            # Check if we have a requirement
            if 'req.txt' in files:
                dest_fd.write("\n\n" + rest.strong("Requirements") + "\n\n")
                dest_fd.write(get_content(os.path.join(root, 'req.txt')))

                # Check if the requirement has some tests (check for tc.txt in
                # subdirs)
                dirs_copy = [k for k in dirs]
                test_id = 1

                for d in dirs_copy:
                    if os.path.isfile(os.path.join(root, d, 'tc.txt')):
                        # There won't be any requirements in that directory
                        dirs.remove(d)

                        dest_fd.write("\n\n" + \
                          rest.strong("Test Case %d: %s" % \
                          (test_id, to_title(d))) + "\n\n")

                        dest_fd.write(get_content(os.path.join(root, d,
                                                               'tc.txt')))
                        test_id += 1

                        # Get the test resources (only ada files in src
                        # subdirectory ?)
                        ada_files = glob.glob(os.path.join(root, d, 'src',
                                                           '*.ad[sb]'))

                        # Keep track of it (used later to write the resources
                        # section)
                        self.resource_list += ada_files
                        dest_fd.write(rest.list([':ref:`%s`' % self.ref(d) \
                                                 for d in ada_files]))

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
