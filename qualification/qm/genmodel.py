#!/usr/bin/env python

# Generate the "model.xml" file from which the qm driven document
# generators will work, with 2 specifities driven by info provided
# on the command line:
#
# * The target do-level, which lets us include level-specific
#   directories that the QM will look into for e.g. TOR artifacts
#   (typical example is the doB specific testcases for DC on non
#   short-circuit operators)
#
# * The target Ada language version, so we can focus the LRM
#   traceability matrix production (and maybe the set of TORs
#   and testcases someday)

import sys, optparse, re
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

RE_LANG="([a-zA-Z]*)([0-9]*)"

def __langdir_for(lang):
    # Requirements for all the versions Ada are all under Ada/ etc;
    # only keep the non-digital start of the language name as the subdir
    # to search:
    return re.match(RE_LANG, string=lang).group(1)
    
def __langversion_for(lang):
    return re.match(RE_LANG, string=lang).group(2)
    
def __reqset_triggers_for (lang, chapter):

    return '\n'.join(
        ('<on_location ',
         '   relative_instance="/TOR/%s"' % __langdir_for(lang),
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

    # We expect one version of Ada to be part of the language set

    ada_lang = [l for l in languages if "Ada" in l][0]

    with open('model.xml', 'w') as f:
        f.write(
            contents_of("template.xml") % {
                'langlevel_reqset_triggers':
                    __langlevel_triggers_for (dolevel=dolevel,
                                              languages=languages),
                'ada_version':
                    __langversion_for(ada_lang),

                'extra_block':
                    __extra_block_for(dolevel=dolevel) 
                }
            )

def __gen_lrm_ref_for(lang):
    """Generate lrm_ref_<adaversion>.txt from lrm_ref.txt, filtering
    out the lines that don't apply to <adaversion> (extracted from the
    input LANGuage name."""

    lang_version = __langversion_for(lang)

    input = open ("LRM/lrm_ref.txt", mode='r')
    output = open ("LRM/lrm_ref_%s.txt" % lang_version, mode='w')

    # The reference matrix lines with section numbers and corresponding
    # language versions are of the form:
    #
    # <secnum> # <versions> # <sectitle>   # <sca_applicable?> # <comment>
    #
    # language versions are shortened to their last two digits,
    # for example:
    #
    # 13.8     # 95,05,12   # Machine Code # yes        #

    # Produce an output file which has all the lines of the input one
    # except those corresponding to sections not for the targeted language.

    short_version = lang_version[-2:]

    for line in input.readlines():

        m = re.match(pattern="([0-9. ]*)#([0-9, ]*?)#", string=line)
        if m and short_version not in m.group(2):
            continue

        output.write(line)

    output.close()
    input.close()
    
# =======================================================================
# ==                          MAIN SCRIPT BODY                         ==
# =======================================================================

valid_dolevels   = ('doA', 'doB', 'doC')
valid_languages  = ('Ada95', 'Ada2005', 'Ada2012')

if __name__ == "__main__":

    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option (
        "--dolevel", dest="dolevel", default=None,
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. One of %s." \
                % valid_dolevels.__str__())
        )

    op.add_option (
        "--languages", dest="languages", default=None,
        help = (
            "comma separated list of languages amongst %s" \
                % valid_languages.__str__())
        )

    (options, args) = op.parse_args()

    exit_if (
        not options.dolevel,
        "Please specify a DO level (--dolevel)."
        )

    exit_if (
        not options.languages,
        "Please specify languages (--languages)."
        )

    languages=options.languages.split(',')

    # should check that there is only one instance of a given
    # language (e.g. only one Ada)

    __gen_model_for(dolevel=options.dolevel, languages=languages)

    [__gen_lrm_ref_for(lang=lang)
     for lang in languages if "Ada" in lang]
