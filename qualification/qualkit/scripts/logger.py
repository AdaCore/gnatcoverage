############################################################################
#                                                                          #
#                       The Qualifying Machine (QM)                        #
#                                                                          #
#                     Copyright (C) 2010-2024, AdaCore                     #
#                                                                          #
# The QM is free software; you can redistribute it  and/or modify it       #
# under terms of the GNU General Public License as published by the Free   #
# Software Foundation; either version 3, or (at your option) any later     #
# version.  The QM is distributed in the hope that it will be useful,      #
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License  for more details. You  should  have  received a copy of the GNU #
# General Public License  distributed with the QM; see file COPYING3. If   #
# not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite #
# 330, Boston, MA 02111-1307, USA.                                         #
#                                                                          #
############################################################################

"""Module providing a message logger"""

from enum import Enum
from functools import total_ordering


@total_ordering
class Log_Level_Kind(Enum):
    DEBUG = 1
    VERBOSE = 2
    INFO = 3
    WARNING = 4
    ERROR = 5

    def __lt__(self, other):
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplementedError


current_level = Log_Level_Kind.VERBOSE


def print_msg(prefix, msg: str) -> None:
    print(prefix + ": " + msg)


def log_error(msg: str) -> None:
    """
    Logs an error message.
    Error messages are always displayed.
    """
    print_msg("ERROR", msg)


def log_warning(msg: str) -> None:
    """
    Logs a warning message.
    Warnings are always displayed

    :param msg: the message
    :type msg: a string
    """
    if current_level <= Log_Level_Kind.WARNING:
        print_msg("WARNING", msg)


def log_info(msg: str) -> None:
    """
    Logs an informative message. Such message is displayed by default, unless
    the QM is run using -v or -d switches.

    :param msg: the message
    :type msg: a string
    """
    if current_level <= Log_Level_Kind.INFO:
        print_msg("INFO", msg)


def log_verbose(msg: str) -> None:
    """
    Logs a message that is only displayed when the QM is run is verbose
    mode or debug mode (respectively -v switch and -d switches).

    :param msg: the message
    :type msg: a string
    """
    if current_level <= Log_Level_Kind.VERBOSE:
        print_msg("VERBOSE", msg)


def log_debug(msg: str) -> None:
    """
    Logs an debug message.
    Such message is not displayed, unless in debug mode (-d switch)

    :param msg: the message
    :type msg: a string
    """
    if current_level <= Log_Level_Kind.DEBUG:
        print_msg("DEBUG", msg)
