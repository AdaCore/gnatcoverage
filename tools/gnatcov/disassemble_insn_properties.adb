------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.Mmap;

with Binary_Files;      use Binary_Files;
with Disassemblers;
with Elf_Disassemblers;
with Hex_Images;
with Highlighting;
with Traces_Elf;        use Traces_Elf;
with Traces;            use Traces;

package body Disassemble_Insn_Properties is

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;
   --  Shortcut to To_Unbounded_String

   function "+" (PC : Pc_Type) return JSON_Value is
     (Create ("0x" & Hex_Images.Hex_Image (PC)));
   --  Shortcut to convert addresses to GNATCOLL.JSON strings "0x...". We
   --  represent addresses as hexadecimal strings in order not to rely on JSON
   --  libraries implementation constraints about abritrarily sized integers.

   Branch_Kind_Strings : constant
     array (Branch_Kind)
     of Ada.Strings.Unbounded.Unbounded_String :=
     (Br_None => +"none",
      Br_Call => +"call",
      Br_Ret  => +"return",
      Br_Jmp  => +"jump");

   -----------------
   -- Disassemble --
   -----------------

   procedure Disassemble
     (Exec_File_Name : String;
      Locations      : User_Locations;
      Compact        : Boolean)
   is
      Exec     : aliased Exe_File_Type'Class := Open_File (Exec_File_Name, 0);
      Exec_Acc : constant Exe_File_Acc := Exec'Unchecked_Access;

      Proc_Locs : Proc_Locations;

      Section_Iter : Addresses_Iterator;
      Section      : Address_Info_Acc;

      Disassembler : access Disassemblers.Disassembler'Class;

      PC       : Pc_Type;
      Insns    : Binary_Content;
      Insn_Len : Integer;

      Result       : constant JSON_Value := Create_Object;
      JSON_Section : JSON_Value := JSON_Null;
      JSON_Insns   : JSON_Array := Empty_Array;

      function Create_Section (Section : Address_Info_Acc) return JSON_Value;
      --  Create, initialize and return a JSON object for Section

      function Create_Instruction (PC : Pc_Type) return JSON_Value;
      --  Create, initialize and return the instruction at PC

      --------------------
      -- Create_Section --
      --------------------

      function Create_Section (Section : Address_Info_Acc) return JSON_Value
      is
         Result : constant JSON_Value := Create_Object;
      begin
         Result.Set_Field ("name", Section.Section_Name.all);
         Result.Set_Field ("low", +Section.First);
         Result.Set_Field ("high", +Section.Last);
         return Result;
      end Create_Section;

      ------------------------
      -- Create_Instruction --
      ------------------------

      function Create_Instruction (PC : Pc_Type) return JSON_Value
      is
         use Highlighting;

         Result                : constant JSON_Value := Create_Object;
         JSON_Tokens           : JSON_Array;

         Insn                  : constant Binary_Content :=
           Slice (Insns, PC, Insns.Last);
         Buffer                : Highlighting.Buffer_Type (128);
         Branch                : Branch_Kind;
         Flag_Indir, Flag_Cond : Boolean;
         Branch_Dest, FT_Dest  : Disassemblers.Dest;
         Mnemonic_Kind         : Some_Token_Kind;

         Cur                   : Cursor;

      begin
         --  Get disassembly tokens and instruction properties

         Disassembler.Disassemble_Insn_Or_Abort
           (Insn, PC, Buffer, Insn_Len, Exec);
         Disassembler.Get_Insn_Properties
           (Insn, PC,
            Branch,
            Flag_Indir, Flag_Cond,
            Branch_Dest, FT_Dest);
         Mnemonic_Kind :=
           Disassemblers.Get_Mnemonic_Kind
             (Branch, Flag_Cond);

         --  Fill the JSON_Tokens array with all tokens

         Cur := Buffer.First;
         loop
            exit when Cur = No_Element;
            declare
               use Ada.Characters.Handling;

               JSON_Token : constant JSON_Value := Create_Object;
               Token_Kind : constant Some_Token_Kind :=
                 (if Token (Cur) = Mnemonic
                  then Mnemonic_Kind
                  else Token (Cur));
            begin
               JSON_Token.Set_Field
                 ("type",
                  To_Lower (Some_Token_Kind'Image (Token_Kind)));
               JSON_Token.Set_Field ("text", Text (Cur));
               Append (JSON_Tokens, JSON_Token);
            end;
            Next (Cur);
         end loop;

         --  Attach various information to the JSON result

         Result.Set_Field ("asm", JSON_Tokens);
         Result.Set_Field ("low", +PC);
         Result.Set_Field
           ("high", +(PC + Pc_Type (Insn_Len) - 1));

         Result.Set_Field ("type", Branch_Kind_Strings (Branch));
         Result.Set_Field ("cond", Flag_Cond);

         if Branch in Br_Call | Br_Jmp then
            Result.Set_Field ("fallthrough-target", +FT_Dest.Target);
            Result.Set_Field ("fallthrough-delay-slot", +FT_Dest.Delay_Slot);

            Result.Set_Field ("indir", Flag_Indir);

            if not Flag_Indir then
               Result.Set_Field ("branch-target", +Branch_Dest.Target);
               Result.Set_Field ("branch-delay-slot", +Branch_Dest.Delay_Slot);
            end if;
         end if;

         return Result;
      end Create_Instruction;

   --  Start of processing for Disassemble

   begin
      Disassembler := Elf_Disassemblers.Disa_For_Machine (Machine);
      Build_Sections (Exec);
      Build_Symbols (Exec);
      Build_Debug_Lines (Exec);

      Translate_Locations (Exec_Acc, Locations, Proc_Locs);

      Init_Iterator (Exec, Section_Addresses, Section_Iter);

      loop
         Next_Iterator (Section_Iter, Section);
         exit when Section = null;

         Load_Section_Content (Exec, Section);
         PC := Section.First;
         Insns := Section.Section_Content;
         JSON_Section := Create_Section (Section);

         while PC <= Insns.Last loop

            Insn_Len := Disassembler.Get_Insn_Length_Or_Abort (Insns);

            if Matches_Locations (Exec_Acc, Proc_Locs, PC) then
               Append (JSON_Insns, Create_Instruction (PC));
            end if;

            --  Get the address of the next instruction to disassemble and stop
            --  when wrapping.

            declare
               Old_PC : constant Pc_Type := PC;
            begin
               PC := PC + Pc_Type (Insn_Len);
               exit when PC < Old_PC;
            end;
         end loop;

         GNATCOLL.Mmap.Free (Section.Section_Region);
         Section.Section_Content := Invalid_Binary_Content;

         if not Is_Empty (JSON_Insns) then
            JSON_Section.Set_Field ("insns", JSON_Insns);
            Result.Set_Field (Section.Section_Name.all, JSON_Section);

            JSON_Section := JSON_Null;
            JSON_Insns := Empty_Array;
         end if;

      end loop;

      Close_File (Exec);
      Put_Line (Result.Write (Compact));
   end Disassemble;

end Disassemble_Insn_Properties;
