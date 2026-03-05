"""Various generation helpers"""

from __future__ import annotations

import contextlib
import os.path
import types


class Environment:
    """
    Memory for recursively visited directories and helper for visiting.

    Visiting a directory creates it if needed, and the guard automagically
    changes the directory back when leaving it.
    """

    class DirectoryGuard(contextlib.AbstractContextManager[None]):
        def __init__(self, env: Environment, subdir: str):
            self.env = env
            self.subdir = subdir

        def __enter__(self) -> None:
            self.env.push_dir(self.subdir)

        def __exit__(
            self,
            exctype: type[BaseException] | None,
            value: BaseException | None,
            traceback: types.TracebackType | None,
        ) -> None:
            self.env.pop_dir()

    def __init__(self) -> None:
        self.dir_stack = [os.getcwd()]

    def get_dir(self, subdir: str) -> Environment.DirectoryGuard:
        """Return a new directory guard for `subdir`."""
        return self.DirectoryGuard(self, subdir)

    def push_dir(self, subdir: str) -> None:
        """Create if needed `subdir` and enter it."""
        new_dir = os.path.join(self.dir_stack[-1], subdir)
        if not os.path.exists(new_dir):
            os.mkdir(new_dir)
        os.chdir(new_dir)
        self.dir_stack.append(new_dir)

    def pop_dir(self) -> None:
        """Leave the most recent visited directory."""
        self.dir_stack.pop()
        os.chdir(self.dir_stack[-1])
