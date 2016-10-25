from datetime import date

project_name = 'GNATcoverage'
project_name_it = '*%s*'% project_name
owner = 'AdaCore'
author = 'AdaCore'

release='DRAFT 0.0'
version='(%s)' % release

copyright = '%s, %d' % (owner, date.today().year)

extensions = ['sphinx.ext.ifconfig']
