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

--  This provides the base IO interface for use by the "bin-file" trace
--  dumper, restricted to configurations where we can assume that a full
--  GNAT RTS is available.
--
--  The interface is devised as a simple subset of Ada.Direct_IO. It may
--  be used from "atexit" handlers though, so may only resort to low level
--  mechanisms as this triggers after the runtime has been finalized.

--  The base IO interface exposed by GNAT.OS_Lib, going through elementary
--  int file descriptors, is well suited for our purpose.

with Interfaces;
with GNAT.OS_Lib;

private package GNATcov_RTS.Traces.Output.Bytes_IO is

   package OSLIB renames GNAT.OS_Lib;
   use type OSLIB.File_Descriptor;

   --  Expose File_Type as a limited record type with implicit
   --  initializers so clients can simply declare a F : File_Type;
   --  object and call Create as with Direct_IO.

   type File_Type is limited record
      FD : OSLIB.File_Descriptor := OSLIB.Invalid_FD;
   end record;

   subtype U8 is Interfaces.Unsigned_8;

   IO_Error : exception;
   --  Exception we raise in case of errors reported by the underlying
   --  IO engine.

   procedure Close (File : in out File_Type);

   procedure Create (File : in out File_Type; Name : String);
   --  Create or reset a file of the requested Name, ready for writing
   --  in binary mode. Raise IO_Error if the operation somehow fails and
   --  would yield an invalid File descriptor.

   procedure Write (File : File_Type; Byte : U8);

end GNATcov_RTS.Traces.Output.Bytes_IO;
