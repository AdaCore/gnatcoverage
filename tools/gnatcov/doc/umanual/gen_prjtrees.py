import re, subprocess

def render(tree):
  subprocess.call(["dot", "-Tpng", "-o"+tree+".png", tree+".dot"])

def gen_tree(tmpl, outfile, selected):
  for l in tmpl:
    m = re.search('([a-z_0-9]*) \[(.*)\];', l)
    if m and m.group(1) in selected:
      outfile.write("\t\t%s [%s,color=darkslateblue, fontcolor=white ];\n" %
        (m.group(1), m.group(2)))
    else:
      outfile.write(l)

tmpl = open("prjtree.dot").readlines()

trees = {
  'Proot': [ 'root' ],
  'Proot-ss_a': [ 'ss_a' ],
  'Proot-root-ss_a': [ 'root', 'ss_a' ],
  'Proot-ss_a-recursive': [ 'ss_a', 'sa1', 'sa2', 'sa3', 'common' ],
  'Proot-ss_a-ss_b': [ 'ss_a', 'ss_b' ],
  'Proot-ss_a-ss_b-recursive': [ 'ss_a', 'sa1', 'sa2', 'sa3',
                                 'ss_b', 'sb1', 'sb2', 'sb3',
                                 'common' ]
}
for k in trees:
  gen_tree(tmpl, open(k + ".dot", "w"), trees[k])
  render(k)

render('prjtree')
