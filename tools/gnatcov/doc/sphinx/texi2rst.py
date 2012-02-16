#!/usr/bin/python
# -*- coding: utf-8 -*-

"""Splits an existing .texinfo file into components suitable for
   makeinfo.py
   If "-node <name>" is specified, only that node and its children are
   kept
"""

import re
import sys
import optparse
import os.path


def finish_section(out, section, section_node, marker, with_label):
    if section_node == '' or section_node == 'Top':
        return

    # Create a label
    if with_label:
        out.write('.. _%s:\n\n' % section_node.replace(' ', '_'))

    # Create header

    if len(marker) == 2:
        out.write(marker[0] * len(section_node) + '\n')

    out.write(section_node + '\n')

    out.write(marker[0] * len(section_node) + '\n\n')

    list_level = 0
    prev_was_blank = False
    in_example = False
    in_table = 0
    in_menu = False
    example_end = ''
    table_marker = '*'

    def word(line, index=1):
        s = line.lstrip().split()
        if len(s) >= index:
            return s[index - 1]
        else:
            return ""

    for line in section.strip().splitlines():
        if word(line, 1) in ('@itemize', '@enumerate'):
            list_level = list_level + 1
            if not prev_was_blank:
                out.write('\n')
            prev_was_blank = False

        elif line.lstrip().startswith('@end itemize') \
                or line.lstrip().startswith('@end enumerate'):
            list_level = list_level - 1
            prev_was_blank = False

        elif word(line, 1) == '@table':
            out.write("\n")
            table_marker = '*'
            in_table += 1
            prev_was_blank = True

        elif in_table > 0 and line.lstrip().startswith('@end table'):
            in_table -= 1
            prev_was_blank = False

        elif line.lstrip().startswith('@menu'):
            out.write('.. toctree::\n')
            out.write('   :numbered:\n')
            out.write('   :maxdepth: 3\n')
            out.write('\n')
            in_menu = True

        elif in_menu:
            if line.startswith('@end menu'):
                in_menu = False
            else:
                entry = re.sub('::.*', '', line)
                entry = re.sub('^\* ', '', entry.strip())
                entry = entry.replace(' ', '_').replace('/', '_')
                out.write('   ' + entry + '\n')

        elif word(line, 1) in (
            "@deffn", "@defmethod", "@deftp", "@deftypemethod",
            "@deffnx", "@defmethodx", "@deftypefn", "@defun"):

            out.write(".. index:: %s\n\n" % line.lstrip().split(' ', 1)[1])
            out.write(line.split(' ', 1)[1].strip() + '\n')

            in_table += 1
            table_marker = '`'

        elif in_table > 0 \
            and word(line, 1) in ("@end") \
            and word(line, 2) in (
               "deffn", "defmethod", "deftp", "deftypemethod",
               "deffnx", "defmethodx", "deftypefn", "defun"):
            in_table -= 1

        elif word(line, 1) in ('@item', '@itemx'):
            line = line.lstrip().replace('@itemx', '')
            line = line.replace('@item', '')

            if in_table > 0:
                if line.strip().startswith(table_marker):
                    # Avoid lines like  "**Bold* text*" which of course
                    # sphinx doesn't like
                    table_marker = ""

                out.write('\n%s%s%s\n' % (
                    table_marker, line.strip(), table_marker))
                prev_was_blank = True
            else:
                out.write('  ' * (list_level - 1) + '* ' + line.strip() + '\n')
                prev_was_blank = False

        elif line.strip() == '':
            if not prev_was_blank:
                out.write('\n')
            prev_was_blank = True

        else:
            if '@example' in line:
                in_example = True
                example_end = '@end example'
                out.write('  ' * list_level + '::\n\n')
                continue
            elif '@smallexample' in line:
                in_example = True
                example_end = '@end smallexample'
                out.write('\n' + '  ' * list_level + '::\n\n')
                continue
            elif '@CODESAMPLE{' in line:
                line = line.replace("@CODESAMPLE{", "")
                example_end = '}'
                in_example = True
                out.write('\n')
                out.write('  ' * list_level + ".. highlight:: ada\n\n")
                out.write('  ' * list_level + '::\n\n')

            elif '@NOTE{' in line:
                line = line.replace("@NOTE{", "")
                in_example = True
                example_end = '}'
                out.write('  ' * list_level + '|Note|::\n\n')

            elif '@TIP{' in line:
                line = line.replace("@TIP{", "")
                in_example = True
                example_end = '}'
                out.write('  ' * list_level + '|Tip|::\n\n')

            elif '@IMPORTANT{' in line:
                line = line.replace("@IMPORTANT{", "")
                in_example = True
                example_end = '}'
                out.write('  ' * list_level + '|Important|::\n\n')

            level = '  ' * (list_level + in_table)

            if in_example and example_end in line:
                in_example = False
                line = line.replace(example_end, '')
                example_end = ''
                out.write(level + '  ' + line.lstrip() + '\n')

            else:
                if in_example:
                    out.write(level + '  ' + line + '\n')
                else:
                    out.write(level + line.lstrip() + '\n')

            prev_was_blank = False

    out.write('\n')


def get_section_marker(line):
    """Return the marker to use in Sphinx for the given section.
       The marker has two characters if overlines are needed.
    """

    if line.startswith('@chapter'):
        return '**'
    elif line.startswith('@section'):
        return '='
    elif line.startswith('@subsection'):
        return '-'
    elif line.startswith('@subsubsection'):
        return '^'
    elif line.startswith('@subsubsubsection'):
        return '"'
    elif line.startswith('@unnumbered'):
        return '~'
    else:
        raise Exception("Unknown section: %s" % line)


def image_name(basename):
    """Return the fullname for the given image"""

    for ext in ("png", "jpg"):
        if os.path.exists('%s.%s' % (basename, ext)):
            return '%s.%s' % (basename, ext)
    raise Exception("Image not found: %s" % basename)

def split(filename, options):
    skip_until = ''
    section = ''
    section_node = ''
    section_need_node = False

    levels = dict(
        chapter=1,
        section=2,
        subsection=3,
        subsubsection=4,
        sec=2,
        subsect=3,
        subsubsec=4,
        unnumbered=1)

    # Stores the @set definitions. They need to be applied in
    # order, since a later definition can reference an earlier
    # one. Hence we use a list

    macros = []

    # These two variables control whether the current text is output or not
    # (it might be filtered by a --node argument). preserve_level indicates
    # when the --node switch matched, and all sections below will be
    # displayed.

    preserve = options.node is None
    preserve_level = 1000  # Level under which we wan

    output = file("index.rst", "w") # Current output file (one per chapter)
    section_marker = '='
    section_node = ''
    prev_line_is_node = False

    content = file(filename).read()
    content = content.replace('\\', '\\\\')
    content = content.replace('@bye', '')
    content = content.replace('@printindex cp', '')
    content = content.replace('@node Index', '')
    content = content.replace('@unnumbered Index', '')
    content = content.replace('@NL{}', '')
    content = content.replace('@include gfdl.texi', '')
    content = content.replace('@copyright{}', 'C')
    content = content.replace('@noindent', '')
    content = content.replace('@dots{}', '...')
    content = content.replace('@result{}', '=>')
    content = content.replace('@error{}', '=> Error: ')
    content = content.replace('@@', '@')
    content = content.replace('@ifhtml', '')
    content = content.replace('@end ifhtml', '')

    content = re.sub("``(.*?)''", "'\\1'", content)

    content = re.compile('@iftex.*?@end iftex', flags=re.DOTALL).sub('', content)

    # Put all hyperlinks on a single line, so that we can do easier
    # substitution later on.

    for regexp in ("@(px)?ref\{.+?\}",
                   "@code\{.+?\}",
                   "@xref\{.+?\}",
                   "@[bi]\{.+?\}"):
        for link in re.finditer(regexp, content, re.DOTALL):
            start = link.start(0)
            end   = link.end(0)
            content = content[:start] \
                    + content[start:end].replace("\n", " ") \
                    + content[end:]

    lines = content.splitlines()
    line_number = 0

    # Use an explicit loop, since in some cases we need to parse several
    # lines, for instance for @smallexample

    while line_number < len(lines):
        line = lines[line_number]
        line_number += 1

        if line == "@c":
            # An empty comment line
            continue

        if skip_until != '':
            if line.find(skip_until) != -1:
                skip_until = ''
            continue

        # The regexps end with [^@] so that "@}" is properly taken as part
        # of the argument, and doesn't end on the "}".

        line = re.sub('@c\s.*', '',       line)   # comments

        line = re.sub('@i\{(.*?)\}',     '*\\1*', line)
        line = re.sub('@emph\{(.*?)\}',  '*\\1*', line)
        line = re.sub('@command\{(.*?)\}',  '*\\1*', line)
        line = re.sub('@option\{(.*?)\}',  '*\\1*', line)
        line = re.sub('@b\{(.*?)\}',     '**\\1**', line)
        line = re.sub('@code\{(.*?[^@])\}',
                      lambda(m): '`%s`' % m.group(1).strip(),
                      line)
        line = re.sub('@var\{(.*?[^@])\}',   '`\\1`', line)
        line = re.sub('@file\{(.*?[^@])\}',  ':file:`\\1`', line)
        line = re.sub('@key\{(.*?[^@])\}',   ':kbd:`\\1`', line)
        line = re.sub('@url\{(.*?[^@])\}',   '`\\1 <\\1>`_', line)
        line = re.sub('@uref\{(.*?[^@])\}',   '`\\1 <\\1>`_', line)
        line = re.sub('@email\{(.*?[^@])\}', '`\\1 <mail:\\1>`_', line)
        line = re.sub('@p?x?ref\{(.*?[^@])\}',
                      lambda(m): ':ref:`%s`' % m.group(1).replace(' ', '_'),
                      line)
        line = re.sub('@cindex (.*)',    '.. index:: \\1\n', line)
        line = re.sub('@anchor\{(.*?[^@])\}',
                      lambda(m): '.. _%s:\n\n' % m.group(1).replace(' ', '_'),
                      line)
        line = re.sub('@image\{([^,}]+)(,[^}]*)?\}',
                      lambda(m): '.. image:: %s\n' % image_name(m.group(1)),
                      line)
        line = line.replace('@{', '{')
        line = line.replace('@}', '}')

        for key, value in macros:
            line = line.replace(key, value)

        if line.startswith('@set '):
            _, name, value = line.rstrip().split(' ', 2)
            macros.append(('@value{%s}' % name, value))

        elif line.startswith('@node'):
            node = line.replace('@node ', '').strip()
            if node.startswith('Top'):
                section = ''
                section_node = node
            prev_line_is_node = True

        elif (line.startswith('@chapter')
             or line.startswith('@section')
             or line.startswith('@unnumbered')
             or line.startswith('@subsection')
             or line.startswith('@subsubsection')):

            # Finish current section
            finish_section(
                output, section, section_node, marker=section_marker,
                with_label=section_need_node)
            section = ''
            section_node = line.strip().split(' ', 1)[1].strip()
            section_need_node = prev_line_is_node

            # Start new section

            if line.startswith('@chapter'):
                output_name = section_node.replace(" ", "_").replace("/", "_")
                output = file('%s.rst' % output_name, "w")

            section_marker = get_section_marker(line)

            level = line[1:].split()[0]
            if level != 'unnumbered':
                level = level.replace('unnumbered', '')
            title = ' '.join(line.split()[1:])  # Rest of the line

            level = levels[level]
            if options.node == title:
                preserve_level = level
                preserve = True
            else:
                preserve = options.node is None or level > preserve_level
                if not preserve:
                    preserve_level = 1000

        elif line.startswith('@menu'):
            if section_node.startswith("Top"):
                section += line + "\n"
            else:
                skip_until = '@end menu'

        elif line.startswith('@detailmenu'):
            skip_until = '@end menu'

        else:
            section += line + "\n"
            prev_line_is_node = False

    if preserve:
        finish_section(
            output, section, section_node, marker=section_marker,
            with_label=prev_line_is_node)


if __name__ == '__main__':
    parse = optparse.OptionParser(
        description='Split a .texi document into components')
    parse.add_option(
        '--node', default=None,
        help='Only preserve that node and its children')

    (options, args) = parse.parse_args()

    for a in args:
        split(a, options)
