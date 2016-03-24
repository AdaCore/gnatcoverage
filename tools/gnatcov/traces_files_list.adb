------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Qemu_Traces; use Qemu_Traces;

package body Traces_Files_List is

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (S       : access Root_Stream_Type'Class;
      Context : access Coverage.Context)
   is
      Context_Info : constant String := Coverage.To_String (Context.all);
   begin
      for TF of Files loop
         String'Output (S, TF.Filename.all);

         --  If this trace file does not come from a checkpoint, then this
         --  context is the original one where it has actually been processed:
         --  record in in its infos.

         if not TF.From_Checkpoint then
            Append_Info (TF.Trace, Coverage_Context, Context_Info);
         end if;

         Checkpoint_Save (S, TF.Trace);
      end loop;

      --  Mark end of list with empty string

      String'Output (S, "");
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (S  : access Root_Stream_Type'Class;
      CS : access Checkpoints.Checkpoint_State)
   is
   begin
      loop
         declare
            Name : constant String := String'Input (S);
            CP_File : Trace_File_Element_Acc;
         begin
            exit when Name = "";

            CP_File := new Trace_File_Element'
              (From_Checkpoint => True,
               Filename        => new String'(Name),
               others          => <>);
            Checkpoint_Load (S, CS, CP_File.Trace);
            Files.Append (CP_File);
         end;
      end loop;
   end Checkpoint_Load;

end Traces_Files_List;
