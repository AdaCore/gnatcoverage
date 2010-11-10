from gnatpython.env import Env
import os

cwd = os.getcwd()
root = 'tests' + os.sep + 'CouvertureQualification'
rootPath = cwd[2:cwd.find (root)-1]
rootPath = rootPath.replace (os.sep, '/')
env = Env()
#env.add_search_path('PYTHONPATH', rootPath)
print rootPath

from test_utils import *

ExerciseAll()
thistest.result()
