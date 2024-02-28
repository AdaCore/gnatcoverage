import re
import subprocess
import os


def render(tree):
    subprocess.call(["dot", "-Tpng", "-o" + tree + ".png", tree + ".dot"])
    subprocess.call(["dot", "-Tpdf", "-o" + tree + ".pdf", tree + ".dot"])


def gen_tree(tmpl, outfile, selected):
    for line in tmpl:
        m = re.search(r"([a-z_0-9]*) \[(.*)\];", line)
        if m and m.group(1) in selected:
            outfile.write(
                "\t\t%s [%s,color=darkslateblue, fontcolor=white ];\n"
                % (m.group(1), m.group(2))
            )
        else:
            outfile.write(line)


tmpl = open("prjtree.dot").readlines()

trees = {
    "Proot-nosub": ["root"],
    "Proot-ss_a-nosub": ["ss_a"],
    "Proot-root-ss_a-nosub": ["root", "ss_a"],
    "Proot-ss_a": ["ss_a", "sa1", "sa2", "sa3", "common"],
}
for k in trees:
    gen_tree(tmpl, open(k + ".dot", "w"), trees[k])
    render(k)
    os.remove (f"{k}.dot")

render("prjtree")
