------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Diagnostics;    use Diagnostics;
with Files_Table;    use Files_Table;
with Outputs;        use Outputs;
with SC_Obligations; use SC_Obligations;
with Subprocesses;   use Subprocesses;
with Types;          use Types;

package body Instrument.Checkpoints is

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS : access Checkpoint_Save_State) is
   begin
      Instrumented_Unit_To_CU_Maps.Map'Write
        (CSS.Stream, Instrumented_Unit_CUs);
      SFI_To_PP_Cmd_Maps.Map'Write
        (CSS.Stream, PP_Cmds);
   end Checkpoint_Save;

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      Instrumented_Unit_CUs.Clear;
      PP_Cmds.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : access Checkpoint_Load_State)
   is
      Relocs    : Checkpoint_Relocations renames CLS.Relocations;
   begin
      declare
         use Instrumented_Unit_To_CU_Maps;
         CP_IU_Map : Map;
      begin
         Map'Read (CLS.Stream, CP_IU_Map);

         for Cur in CP_IU_Map.Iterate loop
            declare
               CP_Unit_Name : constant Compilation_Unit_Name := Key (Cur);
               Existing_Cur : constant Cursor :=
                 Instrumented_Unit_CUs.Find (CP_Unit_Name);
               CU_Ignored   : constant Boolean :=
                 CU_Id_Ignored (Relocs, Element (Cur));

               Existing_CU_Id : constant CU_Id :=
                 (if Existing_Cur = No_Element
                  then No_CU_Id
                  else Element (Existing_Cur));
               New_CU_Id      : constant CU_Id :=
                 (if CU_Ignored
                  then No_CU_Id
                  else Remap_CU_Id (Relocs, Element (Cur)));

            begin
               if not CU_Ignored then
                  if Existing_CU_Id = No_CU_Id then
                     Instrumented_Unit_CUs.Insert (CP_Unit_Name, New_CU_Id);

                  elsif Existing_CU_Id /= New_CU_Id then
                     Warn ("inconsistent information for instrumented unit "
                           & Image (CP_Unit_Name));
                  end if;
               else
                  Report
                    ("Ignored Instrumented Unit from SID file: "
                     & Image (CP_Unit_Name),
                     Kind => Notice);
               end if;
            end;
         end loop;
      end;

      if not Version_Less (CLS, Than => 8) then
         declare
            use SFI_To_PP_Cmd_Maps;
            CP_PP_Map : Map;
         begin
            Map'Read (CLS.Stream, CP_PP_Map);

            for Cur in CP_PP_Map.Iterate loop
               declare
                  SFI          : constant Source_File_Index :=
                    Remap_SFI (Relocs, Key (Cur));
                  Existing_Cur : constant Cursor :=
                    PP_Cmds.Find (SFI);
                  Ignored      : constant Boolean :=
                    SFI_Ignored (Relocs, Key (Cur));

                  Existing_PP_Cmd : constant Command_Type :=
                    (if Existing_Cur = No_Element
                     then Null_Command
                     else Element (Existing_Cur));
                  New_PP_Cmd      : constant Command_Type :=
                    (if Ignored
                     then Null_Command
                     else Element (Cur));

               begin
                  if not Ignored then
                     if Existing_PP_Cmd = Null_Command then
                        PP_Cmds.Insert (SFI, New_PP_Cmd);

                     elsif Existing_PP_Cmd /= New_PP_Cmd then
                        Warn ("inconsistent information for instrumented file "
                              & Get_Unique_Name (SFI));
                     end if;
                  end if;
               end;
            end loop;
         end;
      end if;
   end Checkpoint_Load;

end Instrument.Checkpoints;
