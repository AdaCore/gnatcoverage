# Copyright 2010 - 2011, AdaCore

import qm, qm.osutils
import os, os.path, re, sys, shutil, traceback
import subprocess, difflib
import datetime

class GenRelease(qm.Activity):
   """Generates a new release"""

   def execute(self, link, dest):
      print "\n\n======= Generating the release directory ========="
      plan = qm.Artifact.root().link_dict['plans'].dests[0].link_dict['pdf_plans'].dests[0]
      for f in plan.files:
         if f.mime_type == "application/pdf":
            shutil.copy (f.path, dest.path)
            print f.path + " copied ..."
            break

      return True


class GenEnvironment(qm.Activity):
   """Generates the testing environment"""

   def execute(self, link, dest):
      origin = link.origin
      print "\n\n======= Retrieving the environment ========="
      data = None
      for f in origin.files:
         if f.mime_type == "text/x-python":
            data = f.get_content()
      if data == None:
         assert False, "Cannot determine the environment"
      envdata = eval(data)

      ret = ".. code-block:: text\n\n"
      ret += "  GNAT Pro executable name: %s\n" % (envdata['gnat'])
      ret += "  GNAT Pro version number: %s\n" % (envdata['version'])
      ret += "  Host operating system: %s\n" % (qm.osutils.get_os_version())
      ret += "  Accepted compilation switches:\n"
      for switch in envdata['switches']:
         ret += "    " + switch + "\n"
      ret += "\n"

      f = open(os.path.join(dest.path, "content.rst"), 'w')
      f.write(ret)
      f.close()

      return True
