import os

opcond_section_title = "Operational Conditions of Use"
torintro_section_title = "Document Purpose and Organization"
testproc_section_title = "Overview of the Test Procedures Organization"

rst_prolog = writer.macro('project_command', '``gnatcov``') \
    + writer.role('raw-latex',
                  r'\renewcommand{\labelitemi}{$\bullet$}') + '\n\n' \
    + writer.role('raw-latex',
                  r'\renewcommand{\labelitemii}{$\circ$}') + '\n\n' \
    + writer.role('raw-latex',
                  r'\renewcommand{\labelitemiii}{$\cdots$}') + '\n\n' \
    + writer.role('raw-latex',
                  r'\renewcommand{\labelitemiv}{$-$}') + '\n\n' \
    + writer.macro('adacore', 'AdaCore') \
    + writer.macro('gnatpro', 'GNAT Pro') \
    + writer.macro('gnatpro_it', '*GNAT Pro*') \
    + writer.macro('gnatpro_bold', '**GNAT Pro**') \
    + writer.macro('Ada83', '`Ada 83`') \
    + writer.macro('Ada95', '`Ada 95`') \
    + writer.macro('Ada05', '`Ada 2005`') \
    + writer.macro('Ada12', '`Ada 2012`') \
    + writer.macro('QA', 'Quality Assurance') \
    + writer.macro('plans_pdf', r'``PLANS.pdf``') \
    + writer.macro('tor_pdf', r'``TOR.pdf``') \
    + writer.macro('str_pdf', r'``STR.pdf``') \
    + writer.macro('tqa_file', r'``qa.doc``') \
    + writer.macro('tors', r'Tool Operational Requirements') \
    + writer.macro('plans_doc_title', r'*Qualification Plans*') \
    + writer.macro('tor_doc_title', r'*Tool Operational Requirements and V&V Cases and Procedures*') \
    + writer.macro('str_doc_title', r'*Tool Operational Verification and Validation Results*') \
    + writer.macro('tqa_doc_title', r'*Tool Quality Assurance Records*') \
    + writer.macro('plans_doc', r'*PLANS*') \
    + writer.macro('tor_doc', r'*TOR*') \
    + writer.macro('str_doc', r'*STR*') \
    + writer.macro('tqa_doc', '|tqa_doc_title|') \
    + writer.macro('plans_doc_id', r'*TEC.????-???*') \
    + writer.macro('tor_doc_id', r'*TEC.????-???*') \
    + writer.macro('str_doc_id', r'*TEC.????-???*') \
    + writer.macro('opcond_section_title', r'%s' % opcond_section_title) \
    + writer.macro('opcond_section_title_ref', r'*%s*' % opcond_section_title) \
    + writer.macro('torintro_section_title', r'%s' % torintro_section_title) \
    + writer.macro('torintro_section_title_ref', r'*%s*' % torintro_section_title) \
    + writer.macro('testproc_section_title', r'%s' % testproc_section_title) \
    + writer.macro('testproc_section_title_ref', r'*%s*' % testproc_section_title) \
    + writer.macro('DAL', 'Design Assurance Level') \
    + writer.macro('PSAC', 'Plan for Software Aspects of Certification') \
    + writer.macro('PSAC_bold',
                   '**Plan for Software Aspects of Certification**') \
    + writer.macro('V&V', 'Verification and Validation') \
    + writer.macro('RAMS',
                   'Reliability, Availability, Maintainability and Safety') \
    + writer.macro('ARM', 'Ada Reference Manual') \
    + writer.macro('standard', 'DO-178C/ED-12C') \
    + writer.macro('tool_standard', 'DO-330/ED-215') \
    + writer.macro('client', '`GENERIC CLIENT`')

# We expect to be called through genbundle.py, which should export an
# environment variable stating what --dolevel it was passed ('doA', 'doB',
# or 'doC'). Expose the significant letter to documents:
rst_prolog += writer.macro('dolevel', os.environ.get('GENBUNDLE_DOLEVEL')[-1])
