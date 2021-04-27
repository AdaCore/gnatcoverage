.. Text to be included first in all the descriptions of --target

State the target toolchain configuration used to build the analyzed
programs, as provided to ``gprbuild``. For cross or 32bit native
configurations, this switch together with its possible ``--RTS`` companion
is required for all commands using project files unless the root
project provides the information with ``Target``/``Runtime``
attributes.
