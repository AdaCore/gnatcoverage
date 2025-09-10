############################################################################
#                                                                          #
#                       The Qualifying Machine (QM)                        #
#                                                                          #
#                     Copyright (C) 2010-2014, AdaCore                     #
#                                                                          #
# The QM is free software; you can redistribute it  and/or modify it       #
# under terms of the GNU General Public License as published by the Free   #
# Software Foundation; either version 3, or (at your option) any later     #
# version.  The QM is distributed in the hope that it will be useful,      #
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License  for more details. You  should  have  received a copy of the GNU #
# General Public License  distributed with the QM; see file COPYING3. If   #
# not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite #
# 330, Boston, MA 02111-1307, USA.                                         #
#                                                                          #
############################################################################

"""Provides some high level function to ease generation of ReST strings"""

INDENT = "   "


# paragraph #
def paragraph(content=None):
    """Return a string representing the inlining of a content

    :param content: the directive content
    :type content: either a string or a list of strings
    """
    if content is None:
        return "\n"

    if isinstance(content, str):
        result = content.splitlines()
    else:
        result = content[:]

    result.append("")

    return "\n".join(result) + "\n"


# Directives #
def directive(name, content=None, argument=None, options=None):
    """Return a string representing a directive

    :param name: the directive name
    :type name: a string
    :param content: the directive content
    :type content: either a string or a list of strings
    :param argument: the directive argument (for example the language for
      for a ``code-block`` directive
    :type argument: a string
    :param options: the directive options
    :type options: None or a dictionnary
    :rtype: a string
    """
    result = ["\n\n.. %s::" % name]
    if argument is not None:
        result[0] += " %s" % argument

    if options is not None:
        for option in options:
            if options[option] is None:
                result.append("%s:%s:" % (INDENT, option))
            else:
                result.append("%s:%s: %s" % (INDENT, option, options[option]))

    # add empty line
    result.append("")

    if content is not None:
        # add directive content. content can either be a string or a list
        if isinstance(content, str):
            content = content.splitlines()

        for line in content:
            result.append("%s%s" % (INDENT, line))

        result.append("")

    return "\n".join(result) + "\n"


def code_block(content, language=None):
    """Return a code-block directive string

    :param content: the code to be highlighted
    :type content: string or list of strings
    :param language: the programming language name
    :type language: string
    :rtype: a string
    """
    return directive("code-block", content=content, argument=language)


def toctree(content, hidden=False):
    """Return a toctree directive

    :param content: the list of sub documents
    :type content: list of string
    :param hidden: if True then toctree is hidden
    :type hidden: boolean
    :rtype: a string
    """
    options = {}
    if hidden:
        options["hidden"] = None
    options["maxdepth"] = 1

    return directive("toctree", content=content, options=options)


def csv_table(
    content, headers=None, title=None, widths=None, latex_format=None
):
    """Return a ReST csv-table

    :param content: the elements of the table
    :type content: a list of list of strings
    :param headers: the column titles
    :type headers: a list of strings
    :param title: the table title
    :type title: a string
    :param widths: list of width percentage (for html)
    :type widths: list of integers
    :param latex_format: set the LateX tabular env environment
    :type latex_format: string
    :rtype: a string
    """

    table_content = []
    for line in content:
        table_line = ", ".join(['"%s"' % k.replace('"', '""') for k in line])
        table_content.append(table_line)

    options = {}
    if headers is not None:
        options["header"] = ", ".join(['"%s"' % k for k in headers])
    if widths is not None:
        options["widths"] = ",".join(["%s" % k for k in widths])

    result = ""
    if latex_format is not None:
        result += directive("tabularcolumns", argument=latex_format)
    elif widths is not None:
        latex_w = ["p{%.2f\\textwidth}" % (float(w) / 100.0) for w in widths]
        result += directive(
            "tabularcolumns", argument="|" + "|".join(latex_w) + "|"
        )

    result += directive(
        "csv-table", table_content, argument=title, options=options
    )
    return result


def raw(content, format_tag):
    """Raw output for a given format

    Allows to emit directly latex, html, ... into a sphinx document

    :param content: the raw content
    :type content: a string
    :param format_tag: the selected format (html, latex)
    :type format_tag: a string
    :rtype: a string
    """

    return directive("raw", content=content, argument=format_tag)


def only(content, format_tag):
    """Return a ReST only directive

    :param content: the content
    :type content: a string
    :param format_tag: expression of format tags (html, latex, ...)
    :type format_tag: a string
    :rtype: a string
    """

    return directive("only", content=content, argument=format_tag)


def include(format_tag):
    """Return a ReST include directive"""

    return directive("include", argument=format_tag)


# Roles #
def role(name, argument):
    """Generic function to generate ReST roles

    :param name: the role name
    :type name: a string
    :param argument: the content on which the role is applied
    :type argument: a string
    :rtype: a string
    """
    return ":%s:`%s`" % (name, argument)


def qmref(full_path, name=None):
    """Generate a qmref role

    :param full_path: the target of the ref
    :type full_path: a string
    :param name: intelligible name or description
    :type name: a string
    :rtype: a string
    """

    if name is not None:
        # Protect potential '<' in the explicit text.
        name = name.replace("<", r"\<")
        content = "%s <%s>" % (name, full_path)
    else:
        content = full_path

    return role("doc", content)


def doc(name, explicit_text=None):
    """Generate a doc role

    :param name: name of the doc file
    :type name: a string
    :param explicit_text: intelligible name or description
    :type explicit_text: a string
    :rtype: a string
    """

    # if explicit_text is not None:
    #     # Protect potential '<' in the explicit text.
    #     explicit_text = explicit_text.replace('<', '\<')
    #     content = '%s <%s>' % (explicit_text, name)
    # else:
    # content = name

    return role("doc", name)


def ref(reflabel, explicit_text=None):
    """Generate a ref role

    :param reflabel: the target label to reference
    :type reflabel: a string
    :param explicit_text: intelligible name or description
    :type explicit_text: a string
    :rtype: a string
    """

    if explicit_text is not None:
        # Protect potential '<' in the explicit text.
        explicit_text = explicit_text.replace("<", r"\<")
        content = "%s <%s>" % (explicit_text, reflabel)
    else:
        content = reflabel

    return role("ref", content)


def emphasis(content):
    """Generate an emphasis role equivalent to ``*content*``

    :param content: the content
    :type content: a string
    :rtype: a string
    """
    return role("emphasis", content)


def strong(content):
    """Generate a string role equivalent to ``**content**``

    :param content: the content
    :type content: a string
    :rtype: a string
    """
    return role("strong", content)


def literal(content):
    """Generate an inline code sample

    :param content: the content
    :type content: a string
    :rtype: a string
    """
    return role("literal", content)


# Lists #
def generic_list(content_list, marker="*"):
    """Generate a ReST list

    :param content: the elements of the list
    :type content: a list of strings and list (to generate sublists)
    :param marker: the marker used to mark new elements
    :type marker: a string
    :rtype: a string
    """
    prefix = "%s " % marker
    blank = " " * len(prefix)
    result = []
    for element in content_list:

        if isinstance(element, str):
            element = element.splitlines()

            result.append("%s%s" % (prefix, element[0]))
            for line in element[1:]:
                result.append("%s%s" % (blank, line))
        else:
            # assume element is either a list or a tuple
            result.append("")
            result.append(generic_list(element, " " * len(prefix) + marker))
            result.append("")
    return "\n".join(result) + "\n\n"


def enumerated_list(content_list):
    """Generate an enumerated ReST list

    :param content: the elements of the list
    :type content: a list of strings and list (to generate sublists)
    :rtype: a string
    """
    return generic_list(content_list, marker="#.")


def definition_list_item(definition_name, definition_text):
    """Generate a single definition ReST list

    :param definition_name: The name of the definition
    :type definition_name: a string
    :param definition_text: The text of the definition
    :type definition_text: a string
    :rtype: a string
    """
    res = [definition_name]

    if isinstance(definition_text, str):
        definition_text = definition_text.splitlines()

    for line in definition_text:
        res.append("%s%s" % (INDENT, line))

    return "\n".join(res) + "\n\n"


def definition_list(content_list):
    """Generate a definition ReST list

    :param content: the elements of the list, each element containing first
                    the name of the definition, then the definition itself
    :type content: a list of tuple (string, string)
    :rtype: a string
    """
    result = ""
    for defname, deftext in content_list:
        result += definition_list_item(defname, deftext)

    return result


# Sections #
def heading(title, level=1):
    """Generate a ReST heading

    :param title: the title of that heading
    :type title: a string
    :param level: depth of the heading
    :type level: integer
    :rtype: a string
    """
    markers = ["#", "*", "=", "-", "^", '"']
    marker = markers[level - 1]
    result = [title, marker * len(title)]
    return "\n".join(result) + "\n\n"


def part(title):
    """Equivalent to heading with level set to 1"""
    return heading(title, 1)


def chapter(title):
    """Equivalent to heading with level set to 2"""
    return heading(title, 2)


def section(title):
    """Equivalent to heading with level set to 3"""
    return heading(title, 3)


def subsection(title):
    """Equivalent to heading with level set to 4"""
    return heading(title, 4)


def subsubsection(title):
    """Equivalent to heading with level set to 5"""
    return heading(title, 5)


def paragraph_title(title):
    """Equivalent to heading with level set to 6"""
    return heading(title, 6)


def rubric(title):
    return directive("rubric", argument=title)


def macro(value, expanded):
    """Creates a sphinx macro"""
    return "\n\n.. |%s| replace:: %s\n\n" % (value, expanded)


def macrolatex(value, expanded):
    """Creates a latex specific sphinx macro"""
    return "\n\n.. |%s| raw:: latex\n\n   %s\n\n" % (value, expanded)


def minipage(content, latex_format=r"{\linewidth}"):
    """
    Return a string representing the insertion of a latex minipage
    'around' the given content.
    if not latex generation then, only includes the content as it.

    :param content: the content to be included in minipage
    :type content: either a string or a list of strings
    :param latex_format: the format to be associated with the minipage
                         structure
    :type latex_format: a string
    """

    result = []
    result.append(
        directive(
            "raw",
            ("\\begin{minipage}%s" % latex_format).replace("\\\\", "\\"),
            "latex",
        )
    )

    if content is not None:
        # add mini page content. content can either be a string or a list
        if isinstance(content, str):
            content = content.splitlines()

    result += content

    result.append(directive("raw", r"\end{minipage}", "latex"))

    str_result = "\n".join(result)

    return str_result


def define_role(role_name, options):
    """Allows to define a new role
    then usable through the previous function 'role'

    :param role_name: the name of the role to be defined
    :type role_name: a string,
    :param options: the expected effect of the role
    :type options: a dictionnary or None
    """
    return directive("role", argument=role_name, options=options)
