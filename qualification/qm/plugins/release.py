# Copyright 2010 - 2011, AdaCore

import qm, qm.osutils
import os, os.path, re, sys, shutil, traceback
import subprocess, difflib
import datetime

class GenRelease(qm.Activity):
   """Generates a new release"""

   def execute(self, link, dest):
      print "\n\n======= Generating the release directory ========="
      tmp = qm.archive_in(dest)
      print qm.Artifact.root().link_dict['plans'].dests
      print qm.Artifact.root().link_dict['plans'].dests[0].link_dict['pdf_plans'].dests
      plan = qm.Artifact.root().link_dict['plans'].dests[0].link_dict['pdf_plans'].dests[0]
      for f in plan.files:
         if f.mime_type == "application/pdf":
            shutil.copy (f.path, dest.path)
            print f.path + " copied ..."
            break

      return True
