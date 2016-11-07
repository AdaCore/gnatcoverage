latex_show_pagerefs=True

common_file = os.path.join(
    os.path.dirname(
        os.path.dirname(
            os.path.dirname(
                os.path.dirname(
                    os.path.abspath(__file__))))),
    "common_conf.py")

if os.path.isfile(common_file):
    execfile(common_file)
    execfile(os.path.join(
        os.path.dirname(common_file),
        "qm_prolog.py"))
else:
    print "Couldn't find common configuration file"
    print common_file
    print "from: %s" % __file__

rst_prolog += writer.define_role('raw-html(raw)', {'format': 'html'})

doc_id = "TEC.????-???"

html_sidebars = {
   '**': ['localtoc.html', 'sourcelink.html', 'searchbox.html']
}
