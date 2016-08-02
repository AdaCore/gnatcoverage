import os, sys

print "pre_testscase running from %s" % os.getcwd()

altrun_dir = sys.argv[1] if len(sys.argv) > 1 else None

if altrun_dir:
    print "Altrun directory is " + altrun_dir
