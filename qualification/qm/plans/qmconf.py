version='dev'

# This file gets copied by sphinx as something like:
#    .../qualification/qm/.qm/PLANS/_sphinx/conf.py
#
# and the common configuration file is in qualification/qm ...

common_file = os.path.join(
    os.path.dirname(
        os.path.dirname(
            os.path.dirname(
                os.path.dirname(
                    os.path.abspath(__file__))))),
    "common_conf.py")

if os.path.isfile(common_file):
    execfile(common_file)
else:
    print "Couldn't find common configuration file"
    print common_file
    print "from: %s" % __file__
