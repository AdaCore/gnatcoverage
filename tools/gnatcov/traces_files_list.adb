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

   use Ada.Strings.Unbounded;

   -------------------------------
   -- Create_Trace_File_Element --
   -------------------------------

   function Create_Trace_File_Element
     (Filename : String;
      Kind     : Trace_File_Kind) return Trace_File_Element_Acc
   is
   begin
      return Result : constant Trace_File_Element_Acc := new Trace_File_Element
      do
         Result.Filename := To_Unbounded_String (Filename);
         Result.Kind := Kind;
         Result.Context := Null_Unbounded_String;
      end return;
   end Create_Trace_File_Element;

   ------------------------------
   -- Update_From_Binary_Trace --
   ------------------------------

   procedure Update_From_Binary_Trace
     (Element : in out Trace_File_Element;
      File    : Trace_File_Type)
   is
   begin
      Element.Program_Name := To_Unbounded_String
        (Get_Info (File, Exec_File_Name));
      Element.Time := To_Unbounded_String
        (Format_Date_Info (Get_Info (File, Date_Time)));
      Element.User_Data := To_Unbounded_String
        (Get_Info (File, User_Data));
   end Update_From_Binary_Trace;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context)
   is
      This_Context : constant Unbounded_String := To_Unbounded_String
        (Coverage.To_String (Context.all));
   begin
      for TF of Files loop
         Unbounded_String'Output (CSS, TF.Filename);

         declare
            --  If this trace file does not come from a checkpoint (TF.Context
            --  is empty), then this context is the original one where it has
            --  actually been processed: record in in its infos.

            TF_Context : constant Unbounded_String :=
               (if Length (TF.Context) = 0
                then This_Context
                else TF.Context);
         begin
            if Checkpoints.Version_Less (CSS, Than => 2) then

               --  Before version 2, there were only binary traces and we
               --  streamed metadata as trace infos.

               pragma Assert (TF.Kind = Binary_Trace_File);

               Info_Kind_Type'Write (CSS, Coverage_Context);
               Unbounded_String'Write (CSS, TF_Context);

               Info_Kind_Type'Write (CSS, Exec_File_Name);
               Unbounded_String'Write (CSS, TF.Program_Name);

               Info_Kind_Type'Write (CSS, Date_Time);
               Unbounded_String'Write
                 (CSS,
                  To_Unbounded_String (Parse_Date_Info (To_String (TF.Time))));

               Info_Kind_Type'Write (CSS, User_Data);
               Unbounded_String'Write (CSS, TF.User_Data);

               Info_Kind_Type'Write (CSS, Info_End);

            else
               Trace_File_Kind'Write  (CSS, TF.Kind);
               Unbounded_String'Write (CSS, TF_Context);
               Unbounded_String'Write (CSS, TF.Program_Name);
               Unbounded_String'Write (CSS, TF.Time);
               Unbounded_String'Write (CSS, TF.User_Data);
            end if;
         end;
      end loop;

      --  Mark end of list with empty string

      String'Output (CSS, "");
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State)
   is
   begin
      loop
         declare
            Name    : constant Unbounded_String :=
               Unbounded_String'Input (CLS);
            CP_File : Trace_File_Element_Acc;
         begin
            exit when Length (Name) = 0;
            CP_File := new Trace_File_Element;
            CP_File.Filename := Name;
            Files.Append (CP_File);

            if Checkpoints.Version_Less (CLS, Than => 2) then

               --  Before version 2, there were only binary traces and we
               --  streamed metadata as trace infos.

               CP_File.Kind := Binary_Trace_File;
               CP_File.Context := Null_Unbounded_String;
               CP_File.Program_Name := Null_Unbounded_String;
               CP_File.Time := Null_Unbounded_String;
               CP_File.User_Data := Null_Unbounded_String;

               declare
                  Kind : Info_Kind_Type;
                  Data : Unbounded_String;
               begin
                  loop
                     Info_Kind_Type'Read (CLS, Kind);
                     exit when Kind = Info_End;

                     Unbounded_String'Read (CLS, Data);
                     case Kind is
                        when Exec_File_Name =>
                           CP_File.Program_Name := Data;
                        when Date_Time =>
                           CP_File.Time := To_Unbounded_String
                             (Format_Date_Info (To_String (Data)));
                        when User_Data =>
                           CP_File.User_Data := Data;
                        when Coverage_Context =>
                           CP_File.Context := Data;
                        when others =>
                           null;
                     end case;
                  end loop;
               end;

               --  All traces in checkpoints are supposed to have a coverage
               --  context.

               if Length (CP_File.Context) = 0 then
                  raise Program_Error;
               end if;

            else
               Trace_File_Kind'Read (CLS, CP_File.Kind);
               Unbounded_String'Read (CLS, CP_File.Context);
               Unbounded_String'Read (CLS, CP_File.Program_Name);
               Unbounded_String'Read (CLS, CP_File.Time);
               Unbounded_String'Read (CLS, CP_File.User_Data);
            end if;
         end;
      end loop;
   end Checkpoint_Load;

end Traces_Files_List;
