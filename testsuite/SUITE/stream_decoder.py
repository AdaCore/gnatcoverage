from __future__ import annotations

from collections.abc import Iterable, Iterator
from contextlib import contextmanager
import struct
from typing import IO


def swap_bytes(number: int, size: int) -> int:
    """Swap bytes in ``number``, assumed to be ``size``-bytes large."""
    result = 0
    for _ in range(size):
        result = (result << 8) | (number & 0xFF)
        number = number >> 8
    return result


class ByteStreamDecoder:
    """
    Debug helper to analyze the process of decoding a binary stream.
    """

    BYTES_PER_LINE = 16
    INDENT = "  "

    def __init__(
        self,
        stream: IO[bytes],
        enabled: bool = False,
        max_indent: int = 0,
    ):
        """
        :param stream: Byte stream to read.
        :param enable: If true, enable decoding traces
        :param max_indent: Amount of columns to reserve in logs so that the
            indentation due to label_context does not shift ASCII dumps.
        """
        self.stream = stream
        self.enabled = enabled
        self.label_stack: list[str] = []
        self.offset = 0
        self.max_indent = max_indent

    @property
    def _indent_prefix(self) -> str:
        return self.INDENT * len(self.label_stack)

    def _print(self, message: str, *args: object, **kwargs: object) -> None:
        if not self.enabled:
            return
        if args or kwargs:
            message = message.format(*args, **kwargs)
        print("{}{}".format(self._indent_prefix, message))

    @contextmanager
    def label_context(self, label: str) -> Iterator[None]:
        """
        Context manager to put a label on everything that is read while this
        context manager is active.

        :param str label: Label to use.
        """
        self._print("{} ({:#0x}):", label, self.offset)
        self.label_stack.append(label)
        yield
        self.label_stack.pop()

    def read(self, size: int) -> bytes:
        """
        Read bytes from this file.

        :param size: Number of bytes to read.
        """
        byte_str = self.stream.read(size)
        if self.enabled:
            # Two characters per byte and one space in between. Adjust to
            # account for the maximum indentation.
            bytes_part_size = 3 * self.BYTES_PER_LINE - 1

            reserved_indent = self.max_indent - len(self.label_stack)
            bytes_part_size += max(0, len(self.INDENT) * reserved_indent)

            # Number of lines to represent byte_str
            lines_count = (
                len(byte_str) + self.BYTES_PER_LINE - 1
            ) // self.BYTES_PER_LINE

            for line in range(lines_count):
                start_byte = line * self.BYTES_PER_LINE
                end_byte = (line + 1) * self.BYTES_PER_LINE
                bytes_slice = byte_str[start_byte:end_byte]

                bytes_part = " ".join(
                    "{:02x}".format(b) for b in bytes_slice
                ).ljust(bytes_part_size)

                ascii_part = "".join(
                    chr(ord(".") if b < ord(" ") or b > ord("~") else b)
                    for b in bytes_slice
                )

                self._print("{} | {}".format(bytes_part, ascii_part))
        self.offset += len(byte_str)
        return byte_str


class Struct:
    """
    Wrapper for struct.Struct to work with ByteStreamDecoder.
    """

    def __init__(self, label: str, *fields: tuple[str, str]):
        """
        :param label: Name of this struct.
        :param fields: List of couples (label, format) for the fields of this
            structure.
        """
        self.label = label
        self.le_fields = [
            (name, struct.Struct("<" + fmt)) for name, fmt in fields
        ]
        self.be_fields = [
            (name, struct.Struct(">" + fmt)) for name, fmt in fields
        ]

    def _fields(
        self,
        big_endian: bool = False,
    ) -> list[tuple[str, struct.Struct]]:
        return self.be_fields if big_endian else self.le_fields

    def read(
        self,
        fp: ByteStreamDecoder,
        big_endian: bool = False,
    ) -> dict[str, tuple[int | bytes, ...] | int | bytes] | None:
        """
        Read bytes from ``fp`` and decode these bytes according to the format
        of this structure.

        :param fp: Stream from which to read and decode this structure.
        :param big_endian: Whether to decode structure fields as big endian
            (consider little endian by default).
        """
        with fp.label_context(self.label):
            result = {}
            for i, (name, structure) in enumerate(self._fields(big_endian)):
                with fp.label_context(name):
                    buf = fp.read(structure.size)
                    assert (not buf and i == 0) or len(buf) == structure.size
                    if not buf:
                        return None
                    fields = structure.unpack(buf)
                    result[name] = fields[0] if len(fields) == 1 else fields
            return result

    def write(
        self,
        fp: IO[bytes],
        field_values: dict[str, int | bytes] | Iterable[int | bytes],
        big_endian: bool = False,
    ) -> None:
        fields = self._fields(big_endian)
        values: tuple[int | bytes, ...]

        if isinstance(field_values, dict):
            dict_field_values = field_values
            values = tuple(dict_field_values.pop(name) for name, _ in fields)
            unknown_fields = sorted(dict_field_values)
            if unknown_fields:
                raise ValueError(
                    "Unknown fields: {}".format(" ".join(unknown_fields))
                )
        else:
            values = tuple(field_values)

        if len(values) != len(fields):
            raise ValueError(
                "{} fields expected, got {}".format(len(fields), len(values))
            )

        for value, (_, structure) in zip(values, fields):
            pack_args = value if isinstance(value, tuple) else (value,)
            fp.write(structure.pack(*pack_args))


if __name__ == "__main__":
    assert swap_bytes(0, 1) == 0
    assert swap_bytes(0, 2) == 0
    assert swap_bytes(1, 1) == 1
    assert swap_bytes(1, 2) == 0x100
    assert swap_bytes(0xFF, 2) == 0xFF00
    assert swap_bytes(-1, 2) == 0xFFFF
