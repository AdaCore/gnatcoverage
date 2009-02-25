------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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

--  This package provides a way to accumulate the file names from
--  the command line. These includes:
--  * the list of trace files;
--  * the list of elf files.
--
--  It also provides a set of routines to populate the corresponding database
--  with the information for each element of the list.

with Traces_Dbase; use Traces_Dbase;
with Execs_Dbase; use Execs_Dbase;
with Strings; use Strings;

package Options_Files is

   type Command_Line_Input is private;

   function New_Command_Line_Input return Command_Line_Input;
   --  Allocate a Command_Line_Input and initialize it properly.

   procedure Add_Input
     (Input      : in out Command_Line_Input;
      Trace_File : File_Name;
      Exec_File  : File_Name);
   --  Add an entry to the command line input; this entry associates the name
   --  of a trace file with the name of its corresponding exe. This only
   --  does name association; no coverage computation, not even opening
   --  a file. It just register what trace/elf filenames have been given to
   --  xcov through the command line.

   type Index_Entry is new Natural;

   function Length (Input : Command_Line_Input) return Index_Entry;
   --  Return the length of the Input; that is to say, the number of
   --  trace files that have been registered in it through Add_Input.

   procedure Read_Traces
     (Input : Command_Line_Input;
      Base  : in out Traces_Base);
   --  From the list of trace files recorded in Input, open them all and
   --  populate the trace database with the corresponding informations.

   procedure Open_Execs
     (Input : Command_Line_Input;
      Execs : in out Exec_Base_Type);
   --  From the list of trace files recorded in Input, open them all and
   --  populate the exec database with references to them.

private
   type Command_Line_Input_Type;

   type Command_Line_Input is access Command_Line_Input_Type;

   type Command_Line_Input_Type is record
      --  Linked list that records the pairs of trace/elf files.

      Trace_File : File_Name := null;
      Exec_File  : File_Name := null;
      Index      : Index_Entry := 0;
      Next       : Command_Line_Input := null;
   end record;

end Options_Files;
