import qm


class LRMParser(qm.LocationParser):
    """
    The parser used by the QM to parse lrm_ref file and creates
    LRM section locations.
    """
    def __init__(self):
        self.relpath = None

    def parse(self, path, parent):
        """Main entry point"""

        lineno = 0
        lines = []
        self.parent = parent

        with open(path, 'r') as fd:
            lines = fd.read().splitlines()

        for line in lines:
            lineno += 1

            if lineno > 3:
                print line

                if len(line) > 0:
                    self.append_content_location(line, line, parent)

qm.LocationParser.register_repository_factory(
    'lrm', '*.txt', LRMParser(), False)
