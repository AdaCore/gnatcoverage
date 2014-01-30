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

        lines = []
        self.parent = parent

        with open(path, 'r') as fd:
            lines = fd.read().splitlines()

        for line in lines:

            if line.startswith('#'):
               continue

            if line.startswith('Note'):
                break

            if len(line.strip()) > 0:
                self.append_content_location(line, line, parent)


qm.LocationParser.register_repository_factory(
    'lrm', '*.txt', LRMParser(), False)
