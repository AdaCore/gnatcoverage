#!/usr/bin/env python

import sys, optparse
from gnatpython.ex import Run

def exit_if (p, msg):
    if p:
        print msg
        sys.exit(1)

def contents_of(filename):
    with open(filename) as fd:
        return fd.read()

def __extra_block_for(dolevel):
    """Allow level specific local subsets of testcases."""

    return '\n'.join(
        ('',
         '<artifact_reader class="TC_Set">',
         '  <on_location',
         '     relative_class="TC_Set"',
         '     pattern="${relative.attributes.location}/%s/*/tc_set.rst"' % dolevel,
         '     type="content"',
         '  />',
         '  <creates name="$subst(${attributes.location.basename},([0-9]+_)?(.*),\\2)">',
         '      <assign attribute="location" value="${event.location.container}"/>',
         '  </creates>',
         '</artifact_reader>'))

def __reqset_triggers_for (lang, chapter):
    return '\n'.join(
        ('<on_location ',
         '   relative_instance="/TOR/%s"' % lang,
         '   pattern="${relative.attributes.location}/%s/req_set.rst"' % chapter,
         '   type="content"',
         '/>'))

def __langlevel_triggers_for(dolevel, languages):
    """Triggers so only the language/criterion sections of relevance
    are included."""

    chapters_for = {
        'doC': ['stmt'],
        'doB': ['stmt', 'decision'],
        'doA': ['stmt', 'decision', 'mcdc']
        }

    return '\n'+'\n'.join(
        (__reqset_triggers_for(lang, chapter)
         for lang in languages
         for chapter in chapters_for[dolevel])
        )

def __gen_model_for(dolevel, languages):
    """Generate model.xml from template.xml"""

    with open('model.xml', 'w') as f:
        f.write(
            contents_of("template.xml") % {
                'langlevel_reqset_triggers':
                    __langlevel_triggers_for (dolevel=dolevel,
                                              languages=languages),
                'extra_block':
                    __extra_block_for(dolevel=dolevel) }
            )

# =======================================================================
# ==                          MAIN SCRIPT BODY                         ==
# =======================================================================

valid_dolevels   = ('doA', 'doB', 'doC')

if __name__ == "__main__":

    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option (
        "--dolevel", dest="dolevel", default=None,
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. One of %s." \
                % valid_dolevels.__str__())
        )

    (options, args) = op.parse_args()

    exit_if (
        not options.dolevel,
        "Please specify a DO level (--dolevel)."
        )

    __gen_model_for(dolevel=options.dolevel, languages=["Ada"])
