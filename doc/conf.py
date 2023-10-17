# -*- coding: utf-8 -*-
#
# GNATcoverage documentation build configuration file, created by
# sphinx-quickstart on Fri Dec  9 11:07:19 2011.
#
# This file is execfile()d with the current directory set to its containing
# dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import os

import docutils.nodes as nodes

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
# sys.path.insert(0, os.path.abspath('.'))

# -- General configuration -------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
needs_sphinx = '1.8'
# 1.2 for multiple glossary entries

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = [
    'sphinx.ext.imgmath',
    "sphinx.ext.autodoc",
    "sphinx.ext.coverage",
    "sphinx.ext.doctest",
    "sphinx.ext.intersphinx",
    "sphinx.ext.viewcode",
    "sphinx.ext.todo",
    "sphinx_rtd_theme",
]

# Make sure the target is unique
autosectionlabel_prefix_document = True

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix of source filenames.
source_suffix = '.rst'

# The encoding of source files.
# source_encoding = 'utf-8-sig'

# The master toctree document, named to produce a standard html
# entry point.

master_doc = "index"

# General information about the project.
project = u"GNATDAS"
copyright = u"2009-2022, AdaCore"
docname = u"Manuals"

rst_epilog = """
.. |gcv| replace:: :command:`gnatcov`
.. |gcvrun| replace:: :command:`gnatcov` :command:`run`
.. |gcvcov| replace:: :command:`gnatcov` :command:`coverage`
.. |gcvstp| replace:: :command:`gnatcov` :command:`setup`
.. |gcvins| replace:: :command:`gnatcov` :command:`instrument`
.. |gcvxtr| replace:: :command:`gnatcov` :command:`extract-base64-trace`
.. |gcvcnv| replace:: :command:`gnatcov` :command:`convert`
.. |gcvdsp| replace:: :command:`gnatcov` :command:`disp-routines`
.. |gcvmap| replace:: :command:`gnatcov` :command:`map-routines`

.. |gdas|  replace:: {}
.. |gcp|   replace:: GNATcoverage
.. |gem|   replace:: GNATemulator
.. |gnat|  replace:: GNAT
.. |gpro|  replace:: GNAT Pro
.. |gps|   replace:: GNAT Studio
.. |gtest| replace:: GNATtest

.. |marg| replace:: *(mandatory)*
.. |rarg| replace:: possibly repeated and accepting :term:`@listfile arguments
                    <@listfile argument>`
""".format(
  project
)

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.

# Tool version & release. Nightly setups are expected to store this
# in a version.txt file for us. Assume we're in a development tree otherwise.

version_file = 'version.txt'
if os.path.exists(version_file):
    with open(version_file, 'r') as f:
        version = f.read()
else:
    version = 'dev-tree'

release = version

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
# language = None

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
# today = ''
# Else, today_fmt is used as the format for a strftime call.
# today_fmt = '%B %d, %Y'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build']

# The reST default role (used for this markup: `text`) to use for all
# documents.
# default_role = None

# If true, '()' will be appended to :func: etc. cross-reference text.
# add_function_parentheses = True

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
# add_module_names = True

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
# show_authors = False

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = None

# A list of ignored prefixes for module index sorting.
# modindex_common_prefix = []

# Request figure numbering explicitly. Figure numbering is implicit in
# PDF outputs already, not in html. An explicit request ensures
# consistency across formats and allows the use of :numref: roles,
# which is useful at times.
numfig = True

# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.

html_theme = "sphinx_rtd_theme"
html_sidebars = {
   '**': ['globaltoc.html', 'sourcelink.html', 'searchbox.html']}

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
html_theme_options = {
    "style_nav_header_background": "#12284c",
}

# Add any paths that contain custom themes here, relative to this directory.
# html_theme_path = []

# A short title for the navigation bar.  Default is the same as html_title.
html_short_title = "%s %s [%s]" % (project, docname, version)

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = html_short_title

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
html_logo = "adacore-logo-white.png"

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
html_favicon = "favicon.ico"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = []

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
# html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
# html_use_smartypants = True

# Additional templates that should be rendered to pages, maps page names to
# template names.
# html_additional_pages = {}

# If false, no module index is generated.
html_domain_indices = True

# If false, no index is generated.
html_use_index = True

# If true, the index is split into individual pages for each letter.
# html_split_index = False

# If true, links to the reST sources are added to the pages.
html_show_sourcelink = False

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
# html_show_sphinx = True

# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
# html_show_copyright = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
# html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
# html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = "GNATDASdoc"


# -- Options for LaTeX output -----------------------------------------------

# The paper size ('letter' or 'a4').
# latex_paper_size = 'letter'

# The font size ('10pt', '11pt' or '12pt').
# latex_font_size = '10pt'

# Grouping the document tree into LaTeX files. List of tuples (source start
# file, target name, title, author, documentclass [howto/manual]).

# The version info is displayed on its own on the title page. We don't
# include it in the title here and arrange to have it in the headers with
# to our latex_preamble.

latex_documents = [
  (master_doc, master_doc+'.tex', "%s %s" % (project, docname),
   u'AdaCore', 'manual'),
]

# Get rid of the "Release" tag before the version number
# in the first page subtitle

latex_additional_files = ['latex_preamble.inc']

latex_elements = {
    'releasename': 'Version',
    'preamble': r'\input{latex_preamble.inc}',
    }

# The name of an image file (relative to this directory) to place at the top
# of the title page.
# latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
latex_toplevel_sectioning = 'part'

# If true, show page references after internal links.
# latex_show_pagerefs = False

# If true, show URL addresses after external links.
# latex_show_urls = False

# Additional stuff for the LaTeX preamble.
# latex_preamble = ''

# Documents to append as an appendix to all manuals.
# latex_appendices = []

# If false, no module index is generated.
latex_domain_indices = True


# Custom :cmd-option: role
def cmd_option_role(
    name, rawtext, text, lineno, inliner, options={}, content=[]
):
    node = nodes.literal(rawtext, text, *content, **options)
    return [node], []


def setup(app):
    app.add_role("cmd-option", cmd_option_role)
