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
