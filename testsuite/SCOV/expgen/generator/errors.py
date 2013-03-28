class GenerationError(Exception):
    def __init__(self, context, message):
        super(GenerationError, self).__init__('{}: {}'.format(context, message))
        self.context = context
        self.message = message

class DriverError(GenerationError):
    def __init__(self, topo_dir, test_driver, line_no, message):
        super(DriverError, self).__init__(
            '{}/{}{}'.format(topo_dir, test_driver,
                ':{}'.format(line_no) if line_no is not None else '',
            ),
            message
        )
        self.test_driver = test_driver
        self.line_no = line_no

class BadTopologyError(DriverError):
    def __init__(self, topo_dir, test_driver, line_no, message):
        super(BadTopologyError, self).__init__(
            topo_dir, test_driver, line_no, 'Bad topology: {}'.format(message)
        )
