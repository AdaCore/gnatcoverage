
def isolate (text, pre, post):
    return '\n' * pre + text + '\n' * post

def heading(title, char, pre=2, post=1):
    return isolate (
        pre = pre,
        text = '\n'.join (
            (char * len(title), title, char * len(title))),
        post = post)

def part(title):
    return heading(title, '#')

def chapter(title):
    return heading(title, '*')

def emph(content):
    return "*" + content + "*"

def strong(content):
    return "**" + content + "**"

def list(content, pre=1, post=1):
    return isolate (
        pre  = pre,
        text = '\n'.join([' * %s' % l for l in content]),
        post = post)

def toctree(l, depth=2):
    return isolate (
        pre = 1,
        text = '\n'.join (
            [".. toctree::",
             "   :maxdepth: %s\n" % depth]
            + ["   %s" % item for item in l]),
        post = 1)

