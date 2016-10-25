
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
    + writer.macro('plans_doc', r'*Qualification Plans*') \
    + writer.macro('tor_doc', r'*Tool Operational Requirements and V&V Cases and Procedures*') \
    + writer.macro('str_doc', r'*Tool Operational Verification and Validation Results*') \
    + writer.macro('tqa_doc', r'*Tool Quality Assurance Records*') \
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
