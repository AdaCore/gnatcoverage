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

assignments = {
    'authors'   : ['hainque'],
    'reviewers' : ['guitton', 'fofanov'],
    'approvers' : ['fofanov']}

release='DRAFT 0.0'
version='(%s)' % release

copyright = '%s, %d' % (owner, date.today().year)

extensions = ['sphinx.ext.ifconfig']
