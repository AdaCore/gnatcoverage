------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
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

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Instrument.Common; use Instrument.Common;
with JSON;              use JSON;

package body Instrument.Debug_Dump is

   use Files_Table;

   Units_To_Dump : Unit_Sets.Set;

   ---------------------------
   -- Write_Debug_Dump_File --
   ---------------------------

   procedure Write_Debug_Dump_File (File_Name : String) is
      JSON_Root : constant JSON_Value := Create_Object;

      function Generate_Entry_List_JSON
        (CUs : Unit_Sets.Set) return JSON_Value;
      --  Create a JSON array from an entry map.

      ------------------------------
      -- Generate_Entry_List_JSON --
      ------------------------------

      function Generate_Entry_List_JSON (CUs : Unit_Sets.Set) return JSON_Value
      is
         Result : constant JSON_Value := Create_Object;
      begin
         for CU of CUs loop
            declare
               Entry_Obj           : constant JSON_Value := Create_Object;
               Unit_Buffers_Symbol : constant String := Unit_Buffers_Name (CU);
            begin
               Entry_Obj.Set_Field
                 ("kind",
                  (case CU.Language is
                     when Unit_Based_Language => "unit",
                     when File_Based_Language => "file"));
               Entry_Obj.Set_Field ("buffers", Unit_Buffers_Symbol);

               Result.Set_Field (+CU.Unit_Name, Entry_Obj);
            end;
         end loop;

         return Result;
      end Generate_Entry_List_JSON;

      --  Start of processing for Write_Debug_Dump_File

   begin
      if not Units_To_Dump.Is_Empty then
         JSON_Root.Set_Field
           ("buffer_symbols", Generate_Entry_List_JSON (Units_To_Dump));
      end if;

      Write (File_Name, JSON_Root, Compact => False);
   end Write_Debug_Dump_File;

   --------------------------------------
   -- Register_Buffer_Symbols_For_Unit --
   --------------------------------------

   procedure Register_Buffer_Symbols_For_Unit (CU : Compilation_Unit) is
   begin
      Units_To_Dump.Include (CU);
   end Register_Buffer_Symbols_For_Unit;

end Instrument.Debug_Dump;
