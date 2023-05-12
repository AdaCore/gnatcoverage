# -*- coding: utf-8 -*-
#
# GNATcoverage documentation build configuration file, created by
# sphinx-quickstart on Fri Apr 22 12:56:32 2011.
#
# This file is execfile()d with the current directory set to its containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import sys, os, glob

doc_standard_name = "Tool Operational Verification and Validation Results"

rst_prolog = ".. |str_doc| replace:: *%s*" % doc_standard_name

# Locate and import common_conf.py

work_dir = os.path.join(
    os.path.dirname(
        os.path.dirname(
            os.path.dirname(
                os.path.abspath(__file__))))
)
common_conf_glob = f"{work_dir}/*/qualification/qm/common_conf.py"
common_conf_candidates = glob.glob(common_conf_glob)

assert len(common_conf_candidates) == 1, \
    f"{__file__} couldn't locate lone common_conf.py out of {common_conf_glob}"

common_conf_py = common_conf_candidates[0]
exec(open(common_conf_py).read())

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
# sys.path.insert(0, os.path.abspath('.'))

# -- General configuration -----------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be extensions
# coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = []

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# The suffix of source filenames.
source_suffix = ".rst"

# The encoding of source files.
# source_encoding = 'utf-8-sig'

# The master toctree document.
master_doc = "content"

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
exclude_patterns = []

# The reST default role (used for this markup: `text`) to use for all documents.
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


# -- Options for HTML output ---------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = "default"

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
# html_theme_options = {}

# Add any paths that contain custom themes here, relative to this directory.
# html_theme_path = []

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = "%s Verification and Validation Results" % project_name

# A shorter title for the navigation bar.  Default is the same as html_title.
# html_short_title = None

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
# html_logo = None

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
# html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ["_static"]

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
# html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
# html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
# html_sidebars = {}

# Additional templates that should be rendered to pages, maps page names to
# template names.
# html_additional_pages = {}

# If false, no module index is generated.
# html_domain_indices = True

# If false, no index is generated.
# html_use_index = True

# If true, the index is split into individual pages for each letter.
# html_split_index = False

# If true, links to the reST sources are added to the pages.
# html_show_sourcelink = True

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
htmlhelp_basename = "GNATcoveragedoc"


# -- Options for LaTeX output --------------------------------------------------

# The paper size ('letter' or 'a4').
# latex_paper_size = 'letter'

# The font size ('10pt', '11pt' or '12pt').
# latex_font_size = '10pt'

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass [howto/manual]).
latex_documents = [
    (
        "content",
        "STR.tex",
        "GNATcoverage DO-178C/ED-12C Qualification Material: %s"
        % doc_standard_name,
        "AdaCore",
        "manual",
    ),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
# latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
# latex_use_parts = False

# If true, show page references after internal links.
# latex_show_pagerefs = False

# If true, show URL addresses after external links.
# latex_show_urls = False

# Additional stuff for the LaTeX preamble, in the QM fashion

doc_id = str_doc_id


def project_settings():
    full_document_name = project_name
    if doc_id is not None and len(doc_id) > 0:
        full_document_name = "%s - %s" % (project_name, doc_id)

    return "\n".join(
        [
            r"\newcommand*{\QMFullDocumentName}[0]{"
            + full_document_name
            + r"}",
            r"\newcommand*{\QMProjectName}[0]{" + project_name + r"}",
            r"\newcommand*{\QMDocID}[0]{" + doc_id + r"}",
            r"\newcommand*{\QMVersion}[0]{" + version + r"}",
        ]
    )


latex_preamble = (
    project_settings()
    + r"""
\RequirePackage{lastpage}

\addtocontents{toc}{\protect\thispagestyle{mytoc}}

% Arrange to have explicit notifications on pages
% intentionally left blank
\makeatletter
    \def\cleardoublepage{\clearpage%
        \if@twoside
            \ifodd\c@page\else
                \vspace*{\fill}
                \hfill
                \begin{center}
                \textit{This page intentionally left blank.}
                \end{center}
                \vspace{\fill}
                \thispagestyle{empty}
                \newpage
                \if@twocolumn\hbox{}\newpage\fi
            \fi
        \fi
    }
\makeatother

\makeatletter
\renewcommand{\maketitle}{%
  \begin{titlepage}%
    \let\footnotesize\small
    \let\footnoterule\relax
    \rule{\textwidth}{1pt}%
    \begin{flushright}%
      {\rm\Huge \@title \par}%
      {\em\LARGE\py@HeaderFamily \py@release\releaseinfo \par}
      \vfill
      {\LARGE\py@HeaderFamily
        \begin{tabular}[t]{c}
          \@author \\
          \QMDocID
        \end{tabular}
        \par}
      \vfill\vfill
      {\large
       \@date \par
       \vfill
       \py@authoraddress \par
      }%
    \end{flushright}%\par
    \@thanks
  \end{titlepage}%
  \cleardoublepage%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax
}
\makeatother

\makeatletter
% Redefine the "normal" header/footer style when using "fancyhdr" package:
\@ifundefined{fancyhf}{}{
  % Use \pagestyle{normal} as the primary pagestyle for text.
  \fancypagestyle{normal}{
    \fancyhf{}
    \fancyfoot[LE,RO]{{\py@HeaderFamily\thepage\ of \pageref*{LastPage}}}
    \fancyfoot[LO,RE]{{\py@HeaderFamily \QMFullDocumentName}}
    \fancyhead[LE,RO]{{\py@HeaderFamily \@title\ \QMVersion}}
    \renewcommand{\headrulewidth}{0.0pt}
    \renewcommand{\footrulewidth}{0.4pt}
  }
  % Use \pagestyle{mytoc} as the pagestyle for Table Of Contents.
  \fancypagestyle{mytoc}{
    \fancyhf{}
    \fancyfoot[LO,RE]{{\py@HeaderFamily \QMFullDocumentName}}
    \fancyhead[LE,RO]{{\py@HeaderFamily \@title\ \QMVersion}}
    \renewcommand{\headrulewidth}{0.0pt}
    \renewcommand{\footrulewidth}{0.4pt}
  }
  % Update the plain style so we get the page number & footer line,
  % but not a chapter or section title.  This is to keep the first
  % page of a chapter and the blank page between chapters clean.
  \fancypagestyle{plain}{
    \fancyhf{}
    \fancyfoot[LE,RO]{{\py@HeaderFamily\thepage\ of \pageref*{LastPage}}}
    \fancyfoot[LO,RE]{{\py@HeaderFamily \QMFullDocumentName}}
    \fancyhead[LE,RO]{{\py@HeaderFamily \@title\ \QMVersion}}
    \renewcommand{\headrulewidth}{0.0pt}
    \renewcommand{\footrulewidth}{0.4pt}
  }
}
\makeatother
"""
)


# Documents to append as an appendix to all manuals.
# latex_appendices = []

# If false, no module index is generated.
# latex_domain_indices = True


# -- Options for manual page output -------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ("content", "gnatcoverage", "GNATcoverage Documentation", ["AdaCore"], 1)
]

# flake8: noqa
