# ****************************************************************************
# **                             TEXT LINES AND FILES                       **
# ****************************************************************************

# This module exposes the Tline and Tfile classes to help managing text files
# and their contents.

# Essentially, Tline allows the association of a line number with each line
# and Tfile offers a line hook processing facility.

# ****************************************************************************

class Tline:
    """Associate a line contents with its position in a text file."""
    def __init__(self, lno, text):
        self.lno = lno
        self.text = text

class Tfile:
    """Abstract a set of Tlines from a provided filename, each PROCESSed
    as read at class instanciation time"""

    def __init__(self, filename, process):
        self.nlines = 0
        self.process = process
        self.tlines = [self.new_tline (text)
                       for text in open (filename)]

    def new_tline(self, text):
        self.nlines += 1
        tline = Tline(self.nlines, text)
        self.process (tline)
        return tline

    def contents(self):
        return self.tlines

