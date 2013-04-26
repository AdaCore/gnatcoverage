"""Implement some utility function that ease generation of ReST code"""

HEADING_CHARS = ['#', '*', '=', '-', '^', '"']


def heading(title, heading_char):
    if isinstance(heading_char, int):
        heading_char = HEADING_CHARS[heading_char]

    result = "\n\n"
    result += heading_char * len(title) + '\n'
    result += title + '\n'
    result += heading_char * len(title) + '\n\n'
    return result


def part(title):
    return heading(title, '#')


def chapter(title):
    return heading(title, '*')


def section(title):
    return heading(title, '=')


def subsection(title):
    return heading(title, '-')


def subsubseciton(title):
    return heading(title, '^')


def paragraph(title):
    return heading(title, '"')


def toctree(itemlist, depth=2, attrlist=()):
    return '\n'.join (
        [".. toctree::",
         "   :maxdepth: %s" % depth]
        +
        ["   %s" % attr for attr in attrlist]
        + ['\n'] +
        ["   %s" % item for item in itemlist]
        ) + '\n'


def emphasis(content):
    return "*" + content + "*"


def strong(content):
    return "**" + content + "**"


def generic_block(command, content, command_arg=None):
    if content == '':
        return ''

    if command_arg is None:
        command_arg = ''

    result = "\n.. %s:: %s\n\n" % (command, command_arg)
    result += '\n'.join(['   ' + line for line in content.splitlines()])
    result += '\n\n'
    return result


def warning(content):
    return generic_block('warning', content)


def parsed_literal(content):
    return generic_block('parsed-literal', content)


def code_block(content, language):
    return generic_block('code-block', content, language)


def raw(content, doc_type):
    return generic_block('raw', content, doc_type)


def line_block(content):
    result = '\n\n'
    result += '\n'.join(['   ' + line for line in content.splitlines()])
    return result

def list(content):
    result = '\n\n'
    result += '\n'.join(['    * %s' % l for l in content])
    result += '\n\n'
    return result

