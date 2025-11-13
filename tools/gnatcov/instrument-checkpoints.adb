------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Diagnostics; use Diagnostics;
with Files_Table; use Files_Table;
with Outputs;     use Outputs;

package body Instrument.Checkpoints is

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State) is
   begin
      Write (CSS.all, Instrumented_Unit_CUs);
      Write (CSS.all, PP_Cmds);
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

   procedure Checkpoint_Load (CLS : in out Checkpoint_Load_State) is
      Relocs : Checkpoint_Relocations renames CLS.Relocations;
   begin
      declare
         use Instrumented_Unit_To_CU_Maps;
         CP_IU_Map : Map;
      begin
         Read (CLS, CP_IU_Map);

         for Cur in CP_IU_Map.Iterate loop
            declare
               CP_Unit_Name : constant Compilation_Unit_Part := Key (Cur);
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
                     Warn
                       ("inconsistent information for instrumented unit "
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

      --  Load the mappings for preprocessing commands

      declare
         CP_PP_Cmds : SFI_To_PP_Cmd_Maps.Map;
         CP_SFI     : Source_File_Index;
      begin
         Read (CLS, CP_PP_Cmds);

         for CP_Cur in CP_PP_Cmds.Iterate loop

            --  If this source file is now ignored, just discard its
            --  preprocessing commands.

            CP_SFI := SFI_To_PP_Cmd_Maps.Key (CP_Cur);
            if not SFI_Ignored (Relocs, CP_SFI) then
               declare
                  SFI : constant Source_File_Index :=
                    Remap_SFI (Relocs, CP_SFI);
                  Cur : constant SFI_To_PP_Cmd_Maps.Cursor :=
                    PP_Cmds.Find (SFI);
               begin
                  --  If there was no known preprocessing command for SFI so
                  --  far, just register the loaded one.

                  if not SFI_To_PP_Cmd_Maps.Has_Element (Cur) then
                     PP_Cmds.Insert (SFI, CP_PP_Cmds.Reference (CP_Cur));

                  --  Otherwise, warn if the already known command and the
                  --  loaded one are different.

                  elsif CP_PP_Cmds.Reference (CP_Cur)
                    /= PP_Cmds.Reference (Cur)
                  then
                     Warn
                       ("inconsistent preprocessing information for"
                        & " instrumented file "
                        & Get_Full_Name (SFI));
                  end if;
               end;
            end if;
         end loop;
      end;
   end Checkpoint_Load;

end Instrument.Checkpoints;
