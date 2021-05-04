------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit needs to be compilable with Ada 95 compilers

--  This provides the base IO interface for use by the "base64-stdout" trace
--  dumper, available in all configurations.
--
--  The interface is devised as a simple subset of Ada.Text_IO. It may
--  be used from "atexit" handlers though, so may only resort to low level
--  mechanisms as this triggers after the runtime has been finalized.

--  GNAT.IO provides the low level services we need and is available from
--  most if not all the GNAT runtime profiles so is a good common ground for
--  our use.

with GNAT.IO;

private package GNATcov_RTS.Traces.Output.Text_IO is

   procedure New_Line (N : Positive := 1) renames GNAT.IO.New_Line;
   procedure Put (S : String) renames GNAT.IO.Put;
   procedure Put_Line (S : String) renames GNAT.IO.Put_Line;

end GNATcov_RTS.Traces.Output.Text_IO;
