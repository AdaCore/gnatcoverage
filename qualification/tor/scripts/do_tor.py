"""./do_tor.py

Generate the latex for the TOR document. The scripts looks
for files named req.txt starting from ../../../testsuite/Qualif/Ada.

"""

import sys
import os
import glob

# some constants
ADA_EXT_LEN = len('.ads')
DRIVER_PREFIX = 'test_'
OUTPUT='../tor_impl.tex'
REQ_FILE='req.txt'
TC_FILE='tc.txt'
SRC_DIR='src'
TEST_PY='test.py'
TOR_DIR='../../../testsuite/Qualif/Ada'

# some latex-related commands
INDENT = '\\hspace{1cm} '
INDENT_2 = '\\hspace{2cm} '
# the result to print
res=''

def endsWith (s, postfix):
    return s.rfind(postfix) == len(s) - len(postfix)

def get_path_before_last_sep(path):
    path = path.replace('\\', '/')
    pos = path.rfind('/')
    return path[:pos]

# Return the part of a path before the last separator and
# after the last-1 one
def get_name_before_last_sep(path):
    path = path.replace('\\', '/')
    section_pos_end = path.rfind('/')
    section_pos_start = path.rfind('/', 0, section_pos_end-1)
    section = path[section_pos_start+1:section_pos_end]
    return section

# Return the part of a path after the last separator
def get_name_after_last_sep(path):
    path = path.replace('\\', '/')
    section_pos_start = path.rfind('/')
    section = path[section_pos_start+1:]
    return section

# Open the text file containing the requirement and
# append its content on the result
def get_text(from_path, kind):
    res = ''
    t_path = os.path.join(from_path, kind)
    if os.path.exists(t_path):
        t = open (t_path)
        for line in t:
           res += line
        t.close()
    return res + '\n'

def get_tc_text(from_path):
    return get_text (from_path, TC_FILE)

def get_tor_text(from_path, is_first, is_last):
    res = get_text (from_path, REQ_FILE).replace('Testing strategy',\
                                               '\ \\ \\textbf{Testing strategy: }')
    if not is_last:
        res = '\\textbf{Requirement set}: ' + res
    else:
        if not is_first:
            res = '\\textbf{Requirement}: ' + res

    return res

# Terminal TORs are related to testcases: this is why we
# need to treat them differently
def build_tor (from_path, is_first, is_last):

    res = ''

    # Given a folder, try to find all related testcases,
    # looking also on parent folders. This is useful for the
    # structure of the MC/DC testsuite.
    def get_testcase_list (from_path):
        testcase_num = 0
        testcases = []
        res = ''

        # Given a folder, try to find all testcases it contains
        # by looking for tc.txt files in children folders
        def look_for_testcases(from_path):
            testcases = []
            #print from_path
            for file in glob.glob(from_path + '/*/' + TC_FILE):
                testcases.append(file)
            return testcases

        # a driver is appropriate if in the list of test cases there
        # is one with a similar name
        def is_driver_appropriate (driver, testcases):
            pos_end = driver.rfind('_')
            look_for = driver[len(DRIVER_PREFIX):pos_end]
            for t in testcases:
                the_tc = t[:len(t)-ADA_EXT_LEN]
                if the_tc == look_for:
                    return True
            return False

        def get_testcase_files(from_tc):
            dir = get_path_before_last_sep(from_tc)
            files = {}
            tc_files = []
            drivers = []
            abs_tor_dir = os.path.abspath(TOR_DIR)
            curr_path = dir

            first_turn = True
            while os.path.abspath(curr_path) != abs_tor_dir:
                for f in glob.glob(curr_path + '/src/*.ad*'):
                    files[f] = first_turn
                first_turn = False
                #files.extend(glob.glob(curr_path + '/src/*.ad*'))
                curr_path = os.path.join (curr_path, '..')

            # create a list of all files which could be potential testcases
            # (those not starting with DRIVER_PREFIX)
            short_tcs_tmp = []
            for f in files.keys():
                short = get_name_after_last_sep (f)
                if short.find(DRIVER_PREFIX) == -1:
                    short_tcs_tmp.append(short)

            # eliminate the testcases for which only the spec has been found
            short_tcs = []
            for tc in short_tcs_tmp:
                # if this is a spec
                if endsWith (tc, '.ads'):
                    # then the body should be
                    tc_body = tc.replace ('.ads', '.adb')
                    # look for the body
                    if tc_body in short_tcs_tmp:
                        short_tcs.append(tc)
                else:
                    short_tcs.append(tc)

            # divide all files for the appropriate type
            for f in files.keys():
                short = get_name_after_last_sep (f)
                # if it is a driver
                if short.find(DRIVER_PREFIX) == 0:
                    if files.get(f) or is_driver_appropriate (short, short_tcs):
                        drivers.append(f)
                else:
                    if short in short_tcs:
                        tc_files.append(f)

            return [tc_files, drivers]

        # look inside parent folders until we reach the root
        curr_path = from_path
        abs_tor_dir = os.path.abspath(TOR_DIR)
        testcases = look_for_testcases(curr_path)

        # add the appropriate latex
        testcase_num = len(testcases)

        pos = 0
        for tc in testcases:
            pos = pos + 1
            tc_name = get_name_before_last_sep (tc)
            res += '\paragraph{Test case n. ' + str (pos) + ': ' + tc_name + '}\n'
            res += '\ \\\\'
            res += get_tc_text (get_path_before_last_sep (tc))
            [tc_files, drivers] = get_testcase_files (tc)

            res += INDENT + '\\textbf{\emph{Test data:}}\n'
            if len(tc_files) > 0:
                res += '\\begin{list}{\\labelitemi}{\\leftmargin=2cm}\n'
                for f in tc_files:
                    f_name = get_name_after_last_sep (f)
                    res += '\item ' + f_name + '\n'
                res += '\end{list}\n\n'

            res += INDENT + '\\textbf{\emph{Test scenarios:}}\n'
            if len(drivers) > 0:
                res += '\\begin{list}{\\labelitemi}{\\leftmargin=2cm}\n'
                for f in drivers:
                    f_name = get_name_after_last_sep (f)
                    res += '\item ' + f_name + '\n'
                res += '\end{list}\n\n'

        return res

    res += get_tor_text(from_path, is_first, is_last)
    res += get_testcase_list(from_path)
    return res + '\n'

def print_part(part, root):

    text=''
    zero_nesting_levels = root.split('/')
    zero_nesting = len(zero_nesting_levels)

    # Return how deeply a folder is nested with respect to the root
    def get_nesting_level(path):
        levels = path.split('/')
        return len(levels) - zero_nesting - 2

    # Print latex for a (sub)*section
    def print_section(section, nesting, is_first_section):
        if nesting > 3:
            print 'Problem with nasting at ' + section
        if is_first_section:
            return ''
        section_text = '\\'
        if nesting == 0:
            section_text += 'chapter{' + section + '}\n'
        else:
            for i in range(1,nesting):
                section_text += 'sub'
            section_text += 'section{' + section + '}\n'
        return section_text

    # Do some massaging on a path before producing the latex:
    # extract the name of the section from the path, considering
    # the last part of the dirname after the last separator
    def produce_section(dirpath, f, is_first_section):
        path = os.path.join(dirpath, f)
        section = get_name_before_last_sep(path)
        # normalize
        path = path.replace('\\', '/')
        # remove '_' and replace them with spaces
        section = section.replace('_', ' ')
        nesting_level = get_nesting_level (path)
        return print_section (section, nesting_level, is_first_section)

    text+='\part{' + part +'}\n'

    # Look for requirements and behaves appropriately. If a folder contains
    # 'test.py', 'req.txt' and a folder named 'src', then the requirement is
    # terminal and we need to look for testcases

    is_first_section = True

    for dirpath, dirnames, filenames in os.walk(root, True):
        for f in filenames:
            if f == REQ_FILE:
                req_path = os.path.join(dirpath, REQ_FILE)
                if os.path.exists(req_path):
                    text += produce_section (dirpath, f, is_first_section)
                    derived_requirements = glob.glob(dirpath + '/*/' + REQ_FILE)
                    is_last = len (derived_requirements) == 0
                    text += build_tor (dirpath, is_first_section, is_last)
                    is_first_section = False

    return text

def process_latex(text):
    text=text.replace('_', '\_')
    #text=text.replace('\\', '\\backslash ')
    #text=text.replace('/', '\\backslash ')
    return text

res += print_part ('Statement Coverage', TOR_DIR+'/stmt') + \
    '\n\n'
res += print_part ('MC/DC', TOR_DIR+'/mcdc') + '\n\n'
res = process_latex(res)
out = open(OUTPUT, 'w')
out.write(res)
out.close()
#print res
