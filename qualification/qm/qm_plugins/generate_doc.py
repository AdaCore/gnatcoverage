import os.path
import qm
import qmrest.pdfgenerator

def get_userconf():
       # retrieve the author from the environment data artifact
       env = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'env.py')
       with open(env,"r") as fd:
           envdata = eval(fd.read())

       author = "AdaCore for %s" % (envdata['project_full'])

       return author

class GenPDF(qmrest.pdfgenerator.SphinxGeneratePdf):
    def userconf(self):
        # use method from super just to retrieve the 'extras' field
        [ author, version, extras ] = qmrest.pdfgenerator.SphinxGeneratePdf.userconf (self)
        author = get_userconf()

        return [author, version, extras]

class GenHTML(qmrest.pdfgenerator.SphinxGenerateHtml):
    def userconf(self):
        # use method from super just to retrieve the 'extras' field
        [ author, version, extras ] = qmrest.pdfgenerator.SphinxGenerateHtml.userconf (self)
        author = get_userconf()

        return [author, version, extras]


def _generate(name, path, generator):
    root = None
    top = qm.get_toplevel_artifacts()
    for artifact in top:
        if artifact.name == name:
            root = artifact
            break
    if root == None:
        qm.log_error("Cannot find an artifact named " + name)
    else:
        full = os.path.abspath(path)
        generator.execute(root, full)


def generate_pdf(name, path):
    _generate(name, os.path.join(path, "%s.pdf" % name), GenPDF())


def generate_html(name, path):
    _generate(name, path, GenHTML())
