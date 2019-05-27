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

with Ada.Streams;
with Qemu_Traces; use Qemu_Traces;

package body Traces_Files_List is

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context)
   is
      Context_Info : constant String := Coverage.To_String (Context.all);
   begin
      for TF of Files loop
         String'Output (CSS, TF.Filename.all);
         Trace_File_Kind'Write (CSS, TF.Kind);

         --  If this trace file does not come from a checkpoint, then this
         --  context is the original one where it has actually been processed:
         --  record in in its infos.

         if TF.Context = null then
            String'Output (CSS, Context_Info);
         else
            String'Output (CSS, TF.Context.all);
         end if;

         case TF.Kind is
            when Binary_Trace_File =>
               Checkpoint_Save (CSS, TF.Trace);

            when Source_Trace_File =>
               null;
         end case;
      end loop;

      --  Mark end of list with empty string

      String'Output (CSS, "");
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : access Checkpoints.Checkpoint_Load_State)
   is
      S : constant access Ada.Streams.Root_Stream_Type'Class := CLS.all'Access;
   begin
      loop
         declare
            Name    : constant String := String'Input (CLS);
            Kind    : Trace_File_Kind;
            CP_File : Trace_File_Element_Acc;
         begin
            exit when Name = "";

            --  Before version 2 of the checkpoints format, the only trace
            --  files that existed were binary ones.

            Kind := (if Checkpoints.Version_Less (S, Than => 2)
                     then Binary_Trace_File
                     else Trace_File_Kind'Input (CLS));

            CP_File := new Trace_File_Element (Kind);
            CP_File.Filename := new String'(Name);

            --  Before version 2, the context was stored as a trace info rather
            --  than as a stand-alone field.

            if not Checkpoints.Version_Less (S, Than => 2) then
               CP_File.Context := new String'(String'Input (CLS));
            end if;

            case Kind is
               when Binary_Trace_File =>
                  Checkpoint_Load (CLS, CP_File.Trace);

               when Source_Trace_File =>
                  null;
            end case;

            if not Checkpoints.Version_Less (S, Than => 2) then
               CP_File.Context := new String'
                 (Get_Info (CP_File.Trace, Coverage_Context));
            end if;

            Files.Append (CP_File);
         end;
      end loop;
   end Checkpoint_Load;

end Traces_Files_List;
