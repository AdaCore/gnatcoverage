from collections import OrderedDict
from scripts.common_conf import (
    version,
    project_name,
    owner,
    users,
    assignments,
    plans_doc_id,
    rst_prolog,
    VAR_REPLACEMENTS,
)

doc_id = plans_doc_id

extensions: list[str] = []
papersize = "a4paper"
latex_show_pagerefs = True
html_sidebars = {"**": ["localtoc.html", "sourcelink.html", "searchbox.html"]}
latex_use_parts = False
toc_depth = 2

# roles definition
# ??? This is standard-specific and should not be defined here.
roles = OrderedDict(
    [
        ("authors", {"description": "the authors of the document"}),
        ("reviewers", {"description": "the reviewers of the document"}),
        ("approvers", {"description": "the approvers of the document"}),
    ]
)

# Do some checks
assert rst_prolog is not None, "rst_prolog should be set in common_conf.py"

templates_path = ["_templates"]

# These parameters cannot be overriden so just force their value after
# reading user conf file
master_doc = "index"

source_suffix = ".rst"
project = project_name
exclude_patterns = ["_build", "**/*_bin.rst", "**/*_src.rst"]
pygments_style = "sphinx"
html_theme = "classic"
html_theme_path = ["."]
html_theme_options = {"body_max_width": None}


PAGE_BLANK = r"""
\makeatletter
\def\cleartooddpage{%%
   \cleardoublepage%%
}
\def\cleardoublepage{%%
\clearpage%%
   \if@twoside%%
      \ifodd\c@page%%
         %% nothing to do
      \else%%
         \hbox{}%%
         \thispagestyle{plain}%%
         \vspace*{\fill}%%
         \begin{center}%%
         \textbf{\em This page is intentionally left blank.}%%
         \end{center}%%
         \vspace{\fill}%%
         \newpage%%
         \if@twocolumn%%
            \hbox{}%%
            \newpage%%
         \fi%%
      \fi%%
   \fi%%
}
\makeatother
"""

TOC_DEPTH = (
    r"""
\pagenumbering{arabic}
\setcounter{tocdepth}{%d}
"""
    % toc_depth
)

TOC_CMD = r"""
\makeatletter
\def\tableofcontents{%%
    \pagestyle{plain}%%
    \chapter*{\contentsname}%%
    \@mkboth{\MakeUppercase{\contentsname}}%%
            {\MakeUppercase{\contentsname}}%%
    \@starttoc{toc}%%
}
\makeatother
"""


def latex_hyphen():
    return "\n".join([r"\hyphenpenalty=5000", r"\tolerance=1000"])


def project_settings():

    HLINE = r"\hline"
    HEADERS = r"Name & Company & Team  & Email \\"
    assignment_tab = []

    for role in filter(lambda x: x in assignments, roles.keys()):
        title = (
            r"\multicolumn{4}{l}{\large  \par} \\"
            r" \multicolumn{4}{l}{\large %s \par} \\" % (role.capitalize())
        )

        assignment_tab.append(title)
        assignment_tab.append(HLINE)
        assignment_tab.append(HEADERS)
        assignment_tab.append(HLINE)

        for login in assignments[role]:
            user = users[login]
            assignment_tab.append(
                r"%s & %s & %s & %s \\"
                % (user["name"], user["company"], user["team"], user["email"])
            )

        assignment_tab.append(HLINE)

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
            r"\newcommand*{\QMTabUsers}[0]{"
            + "\n".join(assignment_tab)
            + r"}",
        ]
    )


TOC = r"""
\cleardoublepage
\tableofcontents
\cleardoublepage\pagestyle{plain}
"""

latex_elements = {
    "preamble": TOC_DEPTH
    + PAGE_BLANK
    + TOC_CMD
    + latex_hyphen()
    + "\n"
    + project_settings(),
    "tableofcontents": TOC,
    "papersize": papersize,
}

latex_documents = [
    (
        "index",
        "PLANS.tex",
        f"GNATcoverage DO-178C/ED-12C Qualification Material"
        f": {VAR_REPLACEMENTS["plans_doc_title"]}",
        owner,
        "manual",
    ),
]
