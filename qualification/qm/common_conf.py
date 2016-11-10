from collections import OrderedDict
from datetime import date

project_name = 'GNATcoverage'
project_name_it = '*%s*'% project_name
owner = 'AdaCore'

users = {
    'hainque': {'name': 'Olivier Hainque',
                'company': owner,
                'email': 'hainque@adacore.com',
                'team': 'Qualification'},

    'guitton': {'name': 'Jerome Guitton',
                'company': owner,
                'email': 'guitton@adacore.com',
                'team': 'Qualification'},

    'fofanov': {'name': 'Vasily Fofanov',
                'company': owner,
                'email': 'fofanov@adacore.com',
                'team': 'Qualification'}}

roles = OrderedDict(
    [('authors', {'description': 'the authors of the document'}),
     ('reviewers', {'description': 'the reviewers of the document'})])

assignments = {
    'authors'   : ['hainque'],
    'reviewers' : ['guitton']
}

release='DRAFT 0.0'
version='(%s)' % release

copyright = '%s, %d' % (owner, date.today().year)

extensions = ['sphinx.ext.ifconfig']

