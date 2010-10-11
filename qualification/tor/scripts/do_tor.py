"""./do_tor.py 

Generate the latex for the TOR document. The scripts looks
for files named req.txt starting from ../../../testsuite/Qualif/Ada.

""" 

import sys
import os
import glob

# some constants
TOR_DIR='../../../testsuite/Qualif/Ada'
REQ_FILE='req.txt'
SRC_DIR='src'
TC_PREFIX='test_'

# the result to print
res=''

def print_chapter(chapter, chapter_root):
    
    chapter_text=''
    zero_nesting_levels = chapter_root.split('/')
    zero_nesting = len(zero_nesting_levels) + 1
    
    # Open the text file containing the requirement and
    # append its content on the result
    def get_tor_text(from_path):
        res = ''
        t_path = os.path.join(from_path, REQ_FILE)
        if os.path.exists(t_path):
            t = open (t_path)
            for line in t:
               res += line
            t.close()
        return res + '\n'
    
    # Create a non-terminal TOR: simply add the corresponding
    # text
    def build_non_terminal_tor (from_path):
        res = 'This is a non-terminal TOR\n\n'
        res += get_tor_text (from_path)
        return res
    
    # Terminal TORs are related to testcases: this is why we
    # need to treat them differently
    def build_tor (from_path):
        
        res = 'This is a terminal TOR\n\n'
        
        # Given a folder, try to find all related testcases, 
        # looking also on parent folders. This is useful for the
        # structure of the MC/DC testsuite.
        def get_testcase_list (from_path):
            testcase_num = 0
            testcases = []
            res = ''
            
            # Given a folder, try to find all testcases it contains
            # by looking for files named 'test_*' inside the 'src'
            # subfolder. Returns a list of string containing the path
            # to the testcases (.adb files)
            def look_for_testcases(from_path):
                src_path = os.path.join(from_path, SRC_DIR)
                testcases = []
                if os.path.exists(src_path):
                    for src_file in glob.glob(src_path + '/' + TC_PREFIX + '*'):
                        the_path = src_file.replace('\\', '/')
                        testcases.append(the_path)
                return testcases
           
            # look inside parent folders until we reach the root         
            curr_path = from_path
            abs_tor_dir = os.path.abspath(TOR_DIR)
            while os.path.abspath(curr_path) != abs_tor_dir:
                testcases.extend(look_for_testcases(curr_path))
                curr_path = os.path.join (curr_path, '..')
            
            # add the appropriate latex
            testcase_num = len(testcases)
            if testcase_num > 0:
                res += '\paragraph*{Derived testcases}\n'
                res += '\\begin{enumerate}\n'
                for tc in testcases:
                    res += '\item ' + tc + '\n'
                res += '\end{enumerate}\n'
                res += '\n'
            return res
        
        res += get_tor_text(from_path)
        res += get_testcase_list(from_path)
        return res + '\n'
    
    # Return how deeply a folder is nested with respect to the root
    def get_nesting_level(path):
        levels = path.split('/')
        return len(levels) - zero_nesting
    
    # Print latex for a (sub)*section
    def print_section(section, nesting):
        section_text = '\\'
        for i in range(0,nesting):
            section_text += 'sub'
        section_text += 'section{' + section + '}\n'
        return section_text
    
    # Do some massaging on a path before producing the latex:
    # extract the name of the section from the path, considering
    # the last part of the dirname after the last separator
    def produce_section(dirpath, f):
        path = os.path.join(dirpath, f)
        # normalize
        path = path.replace('\\', '/')
        section_pos_end = path.rfind('/')
        section_pos_start = path.rfind('/', 0, section_pos_end-1)
        section = path[section_pos_start+1:section_pos_end]
        # remove '_' and replace them with spaces
        section = section.replace('_', ' ')
        nesting_level = get_nesting_level (path)
        return print_section (section, nesting_level)
    
    chapter_text+='\chapter{' + chapter +'}\n' 
    
    # Look for requirements and behaves appropriately. If a folder contains
    # both 'req.txt' and a folder named 'src', then the requirement is
    # terminal and we need to look for testcases
    for dirpath, dirnames, filenames in os.walk(chapter_root, True):
        for f in filenames:
            if f == REQ_FILE:
                src_path = os.path.join(dirpath, SRC_DIR)
                chapter_text += produce_section(dirpath, f)
                if os.path.exists(src_path):
                    chapter_text += build_tor (dirpath)
                else:
                    chapter_text += build_non_terminal_tor(dirpath)
                    
    return chapter_text
                        
#res += print_chapter ('Statement Coverage', TOR_DIR+'/stmt/IsolatedConstructs') + '\n\n'
res += print_chapter ('MC/DC', TOR_DIR+'/mcdc') + '\n\n'
print res