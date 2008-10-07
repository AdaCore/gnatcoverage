------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
with Traces_Dbase; use Traces_Dbase;

package Traces_Files is
   --  This exception is raised if the trace file is invalid or corrupted.
   Bad_File_Format : exception;

   --  This exception is raise in case of OS error during write.
   Write_Error : exception;

   --  Load in memory (and possibly merge) a trace file.
   procedure Read_Trace_File (Base : in out Traces_Base; Filename : String);

   --  Write traces to a file.
   --  Always generate a consolidated file.
   procedure Write_Trace_File (Base : Traces_Base; Filename : String);

   --  Raw dump of a trace file.
   procedure Dump_Trace_File (Filename : String);

   --  Add coverage annotations to the objdump disassembly output.
   --  Read objdump output from standard input.
   --  procedure Annotate_Objdump;

end Traces_Files;
