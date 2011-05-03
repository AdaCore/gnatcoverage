
def heading(title, heading_char, pre=2, post=1):

    return (
        '\n' * pre
        + heading_char * len(title) + '\n'
        + title + '\n'
        + heading_char * len(title) + '\n'
        + '\n' * post)

def part(title):
    return heading(title, '#')

def chapter(title):
    return heading(title, '*')

