from collections import OrderedDict
from datetime import date

project_name = "GNATcoverage"
project_name_it = "*%s*" % project_name
owner = "AdaCore"

users = {
    "unassigned": {
        "name": "Unassigned",
        "company": "",
        "email": "",
        "team": "",
    },
    "hainque": {
        "name": "Olivier Hainque",
        "company": owner,
        "email": "hainque@adacore.com",
        "team": "Qualification",
    },
    "guitton": {
        "name": "Jerome Guitton",
        "company": owner,
        "email": "guitton@adacore.com",
        "team": "Qualification",
    },
    "leguillou": {
        "name": "Erwan Leguillou",
        "company": owner,
        "email": "leguillou@adacore.com",
        "team": "Qualification",
    },
    "fofanov": {
        "name": "Vasily Fofanov",
        "company": owner,
        "email": "fofanov@adacore.com",
        "team": "Qualification",
    },
}

roles = OrderedDict(
    [
        ("authors", {"description": "the authors of the document"}),
    ]
)

assignments = {"authors": ["hainque"]}

release = "DRAFT 0.0"
version = "(version %s)" % release

copyright = "%s, %d" % (owner, date.today().year)  # noqa: A001

extensions = ["sphinx.ext.ifconfig"]

# Parameters - which could be templated and instantiated
# from parameters known at higher levels of the kit production
# process:

tor_doc_id = "PE.<TOR-DOC-TN>"
str_doc_id = "PE.<STR-DOC-TN>"
plans_doc_id = "PE.<PLANS-DOC-TN>"

gnatpro_version = "GNATPRO.X"
gprbuild_version = "GPRBUILD.Y"
gnatcov_version = "GNATCOV.Z"

opcond_section_title = "Operational Conditions of Use"
torintro_section_title = "Document Purpose and Organization"
testproc_section_title = "Overview of the Test Procedures Organization"

VAR_REPLACEMENTS = {
    "project_name": project_name,
    "project_name_bold": "**%s**" % project_name,
    "project_name_it": "*%s*" % project_name,
    "current_version": version,
    "project_command": "``gnatcov``",
    "adacore": "AdaCore",
    "gnatpro": "GNAT Pro",
    "gnatpro_it": "*GNAT Pro*",
    "gnatpro_bold": "**GNAT Pro**",
    "Ada83": "`Ada 83`",
    "Ada95": "`Ada 95`",
    "Ada05": "`Ada 2005`",
    "Ada12": "`Ada 2012`",
    "Ada22": "`Ada 2022`",
    "QA": "Quality Assurance",
    "plans_pdf": r"``PLANS.pdf``",
    "tor_pdf": r"``TOR.pdf``",
    "str_pdf": r"``STR.pdf``",
    "tqa_file": r"``qa.doc``",
    "tors": r"Tool Operational Requirements",
    "plans_doc_title": r"*Qualification Plans*",
    "tor_doc_title": (
        r"*Tool Operational Requirements and V&V Cases and Procedures*"
    ),
    "str_doc_title": r"*Tool Operational Verification and Validation Results*",
    "tqa_doc_title": r"*Tool Quality Assurance Records*",
    "plans_doc": r"*PLANS*",
    "tor_doc": r"*TOR*",
    "str_doc": r"*STR*",
    "tqa_doc": "|tqa_doc_title|",
    "plans_doc_id": r"*" + plans_doc_id + r"*",
    "tor_doc_id": r"*" + tor_doc_id + r"*",
    "str_doc_id": r"*" + str_doc_id + r"*",
    "gnatcov_version": r"%s" % gnatcov_version,
    "gnatpro_version": r"%s" % gnatpro_version,
    "gprbuild_version": r"%s" % gprbuild_version,
    "opcond_section_title": r"%s" % opcond_section_title,
    "opcond_section_title_ref": r"*%s*" % opcond_section_title,
    "torintro_section_title": r"%s" % torintro_section_title,
    "torintro_section_title_ref": r"*%s*" % torintro_section_title,
    "testproc_section_title": r"%s" % testproc_section_title,
    "testproc_section_title_ref": r"*%s*" % testproc_section_title,
    "DAL": "Design Assurance Level",
    "PSAC": "Plan for Software Aspects of Certification",
    "PSAC_bold": "**Plan for Software Aspects of Certification**",
    "V&V": "Verification and Validation",
    "ARM": "Ada Reference Manual",
    "standard": "DO-178C/ED-12C",
    "tool_standard": "DO-330/ED-215",
    "client": "`GENERIC CLIENT`",
    "current_authors": (
        owner
        if not assignments
        else ", ".join([name.capitalize() for name in assignments["authors"]])
    ),
    "release": release,
    "version": version,
}

rst_prolog = """
.. |LF| raw:: latex

   \\\\

.. |newline| raw:: latex

   \\newline



.. |nbsp| raw:: latex

   ~



.. |pagebreak| raw:: latex

   \\newpage

.. role:: raw-latex(raw)
   :format: latex

:raw-latex:`\\renewcommand{\\labelitemi}{$\\bullet$}`
:raw-latex:`\\renewcommand{\\labelitemii}{$\\circ$}`
:raw-latex:`\\renewcommand{\\labelitemiii}{$\\cdots$}`
:raw-latex:`\\renewcommand{\\labelitemiv}{$-$}`

.. role:: raw-html(raw)
   :format: html
""" + "\n\n".join(
    [
        f".. |{key}| replace:: {VAR_REPLACEMENTS[key]}"
        for key in VAR_REPLACEMENTS
    ]
)
