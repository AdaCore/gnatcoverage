import qmrest.pdfgenerator, qm, re

class GnatCheckPdfGenerator(qmrest.pdfgenerator.PdfGenerator):

    def userconf(self):
       # use method from super just to retrieve the 'extras' field
       [ author, version, extras ] = qmrest.pdfgenerator.PdfGenerator.userconf (self)

       # retrieve the author from the environment data artifact
       env = qm.Artifact.root().files[0]
       fd = open(env.path,"r")
       cnt = fd.read()
       fd.close()
       envdata = eval(cnt)

       author = "AdaCore for %s" % (envdata['project_full'])

       # now retrieve the official version number directly from the document
       if self.origin.name == "TOR":
          doc_path = self.origin.link_dict['history'].dests[0].files[0].path
       else:
          doc_path = ""
          for f in self.origin.files:
             if f.mime_type == "text/x-rst":
                doc_path = f.path
                break

       if doc_path != "":
          fd = open(doc_path,"r")
          cnt = fd.read()
          fd.close()
          found_table = False
          skipped_header = False
          for l in cnt.splitlines():
             if not found_table:
                if re.match(r"^[.][.] csv-table.* History",l) != None:
                   found_table = True
             elif not skipped_header:
                if len(l) == 0:
                   skipped_header = True
             elif len(l) == 0:
                break
             else:
                version = ""
                for c in l:
                   if c == '|':
                      break
                   if c != ' ':
                      version += c

       return [author, version, extras]
