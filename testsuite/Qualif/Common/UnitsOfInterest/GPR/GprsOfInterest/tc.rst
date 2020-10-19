**Check the Projects Of Interest determination rules**

Check that the selection of Projects Of Interest with
-P/--projects/--no-subprojects works as intended, with or without projects
marked Externally_Built.

Exercize various combinations of the options on a hierarchy of projects, some
possibly claimed ``Externally_Built`` in accordance with a scenario variable.

The hierarchy of projects is depicted below, with a --> b denoting a "with b"
in project file a::

                +-> ssa.gpr -> a1.gpr -+
     root.gpr --|                      |--> common.gpr
                +-> ssb.gpr -> b1.gpr -+


"ssa" and "ssb" stand for "subsystem A" and "subsystem B", respectively, and
each can be advertised Externally_Built by setting the SSA_X or SSB_X external
variable to "True" from the command line.
