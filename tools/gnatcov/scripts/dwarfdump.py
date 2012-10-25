# Usage: dwarfdump.py objdump -Dr <exe>
# Annotates the output of objdump with sloc info from exe

import subprocess, re, sys

def get_dwarf(exe):
  line_info = {}
  proc = subprocess.Popen(['gnatcov','dump-lines',exe],stdout=subprocess.PIPE)
  outs, errs = proc.communicate()
  for l in outs.split('\n'):
    m = re.match ("([0-9a-f]+)-[0-9a-f]+ line (.*)", l)
    if m:
      addr, line = m.group(1), m.group(2)
      if not addr in line_info:
        line_info[addr] = []
      if not line in line_info[addr]:
        line_info[addr].append(line)
  return line_info

def do_dump(line_info, cmd):
  print "Executing: " + str(cmd)
  proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
  outs, errs = proc.communicate()
  linfo = []
  for l in outs.split('\n'):
    m = re.match("( *[0-9a-f]+):", l)
    if m:
      a =  m.group(1).replace (' ', '0')
      if a in line_info:
        this_linfo = line_info[a]
        if this_linfo != linfo:
          print ""
          for li in this_linfo:
            print ">>> " + li
          linfo = this_linfo
    print l

if __name__ == "__main__":
  do_dump(get_dwarf(sys.argv[-1]), sys.argv[1:])

