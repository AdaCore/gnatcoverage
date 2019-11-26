from contextlib import contextmanager
import struct


class ByteStreamDecoder(object):
    """
    Debug helper to analyze the process of decoding a binary stream.
    """

    BYTES_PER_LINE = 16
    INDENT = '  '

    def __init__(self, stream, enabled=False, max_indent=0):
        """
        :param file stream: Byte stream to read.
        :param bool enable: If true, enable decoding traces
        :param int max_indent: Amount of columns to reserve in logs so that
            the indentation due to label_context does not shift ASCII dumps.
        """
        self.stream = stream
        self.enabled = enabled
        self.label_stack = []
        self.offset = 0
        self.max_indent = max_indent

    @property
    def _indent_prefix(self):
        return self.INDENT * len(self.label_stack)

    def _print(self, message, *args, **kwargs):
        if not self.enabled:
            return
        if args or kwargs:
            message = message.format(*args, **kwargs)
        print('{}{}'.format(self._indent_prefix, message))

    @contextmanager
    def label_context(self, label):
        """
        Context manager to put a label on everything that is read while this
        context manager is active.

        :param str label: Label to use.
        """
        self._print('{} ({:#0x}):', label, self.offset)
        self.label_stack.append(label)
        yield
        self.label_stack.pop()

    def read(self, size):
        """
        Read bytes from this file.

        :param int size: Number of bytes to read.
        :rtype: str
        """
        bytes = self.stream.read(size)
        if self.enabled:
            # Two characters per byte and one space in between. Adjust to
            # account for the maximum indentation.
            bytes_part_size = 3 * self.BYTES_PER_LINE - 1

            reserved_indent = self.max_indent - len(self.label_stack)
            bytes_part_size += max(0, len(self.INDENT) * reserved_indent)

            # Number of lines to represent bytes
            lines_count = (len(bytes) + self.BYTES_PER_LINE
                           - 1) // self.BYTES_PER_LINE

            for line in range(lines_count):
                start_byte = line * self.BYTES_PER_LINE
                end_byte = (line + 1) * self.BYTES_PER_LINE
                bytes_slice = bytes[start_byte:end_byte]

                bytes_part = ' '.join(
                    '{:02x}'.format(ord(b)) for b in bytes_slice
                ).ljust(bytes_part_size)

                ascii_part = ''.join(
                    '.' if b < ' ' or b > '~' else b
                    for b in bytes_slice
                )

                self._print('{} | {}'.format(bytes_part, ascii_part))
        self.offset += len(bytes)
        return bytes


class Struct(object):
    """
    Wrapper for struct.Struct to work with ByteStreamDecoder.
    """

    def __init__(self, label, *fields):
        """
        :param str label: Name of this struct.
        :param fields: List of couples (label, format) for the fields of this
            structure.
        :type fields: list[(str, str)]
        """
        self.label = label
        self.fields = [(name, struct.Struct(fmt)) for name, fmt in fields]

    def read(self, fp):
        """
        Read bytes from ``fp`` and decode these bytes according to the format
        of this structure.

        :param ByteStreamDecoder fp: Stream from which to read and decode this
            structure.
        """
        with fp.label_context(self.label):
            result = {}
            for i, (name, structure) in enumerate(self.fields):
                with fp.label_context(name):
                    buf = fp.read(structure.size)
                    assert (not buf and i == 0) or len(buf) == structure.size
                    if not buf:
                        return None
                    field = structure.unpack(buf)
                    if len(field) == 1:
                        field = field[0]
                    result[name] = field
            return result

    def write(self, fp, field_values):
        if isinstance(field_values, dict):
            dict_field_values = field_values
            field_values = [dict_field_values.pop(name)
                            for name, _ in self.fields]
            unknown_fields = sorted(dict_field_values)
            if unknown_fields:
                raise ValueError('Unknown fields: {}'
                                 .format(' '.join(unknown_fields)))

        if len(field_values) != len(self.fields):
            raise ValueError('{} fields expected, got {}'
                             .format(len(self.fields), len(field_values)))

        print(field_values)
        for value, (_, structure) in zip(field_values, self.fields):
            fp.write(structure.pack(value))
