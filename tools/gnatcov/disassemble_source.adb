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

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

with GNATCOLL.Mmap;

with Disassemblers;
with Elf_Disassemblers;
with Hex_Images;        use Hex_Images;
with Highlighting;
with Traces_Elf;        use Traces_Elf;
with Traces;            use Traces;

package body Disassemble_Source is

   -----------------
   -- Disassemble --
   -----------------

   procedure Disassemble
     (Exec_File_Name : String;
      Locations      : User_Locations;
      Format         : Output_Format)
   is
      Exec     : aliased Exe_File_Type;
      Exec_Acc : constant Exe_File_Acc := Exec'Unchecked_Access;

      Proc_Locs : Proc_Locations;

      Section_Iter, Symbol_Iter                        : Addresses_Iterator;
      Section, Symbol, Last_Symbol, Last_Output_Symbol : Address_Info_Acc;

      Disassembler : access Disassemblers.Disassembler'Class;

      PC       : Pc_Type;
      Insns    : Binary_Content;
      Insn_Len : Integer;
      Buffer   : Highlighting.Buffer_Type (128);

      Branch                : Branch_Kind;
      Flag_Indir, Flag_Cond : Boolean;
      Branch_Dest, FT_Dest  : Disassemblers.Dest;

      Mnemonic_Kind : Highlighting.Some_Token_Kind;

      procedure Put_Token (S : String; Kind : Highlighting.Some_Token_Kind);
      --  For text format, print Text as-is. Print a serialized token
      --  otherwise.

      procedure Output_Instruction;
      --  Print the current instruction

      ---------------
      -- Put_Token --
      ---------------

      procedure Put_Token (S : String; Kind : Highlighting.Some_Token_Kind)
      is
         use Highlighting;

      begin
         if Format = Text then
            Put (S);
         else
            Put_Line
              (Format_Token
                 (S,
                  (if Kind = Mnemonic
                   then Mnemonic_Kind
                   else Kind)));
         end if;
      end Put_Token;

      ------------------------
      -- Output_Instruction --
      ------------------------

      procedure Output_Instruction
      is
         use Highlighting;

         PC_Diff     : constant Pc_Type :=
           PC - (if Last_Symbol = null then 0 else Last_Symbol.First);
         PC_Diff_Img : String := "   " & Hex_Image (PC_Diff);

         Cur         : Cursor;

      begin
         if Last_Symbol /= Last_Output_Symbol then
            Put_Token (Last_Symbol.Symbol_Name.all, Name);
            Put_Token (":", Punctuation);
            Put_Token ((1 => ASCII.LF), Text);
            Last_Output_Symbol := Last_Symbol;
         end if;

         --  Turn PC_Diff_Img into a space left-padded "+0xXYZ" hexadecimal
         --  number.

         if PC_Diff = 0 then
            PC_Diff_Img := (others => ' ');
            PC_Diff_Img (PC_Diff_Img'Last - 3 .. PC_Diff_Img'Last) := "+0x0";

         else
            for I in PC_Diff_Img'First + 3 .. PC_Diff_Img'Last loop
               if PC_Diff_Img (I) = '0' then
                  PC_Diff_Img (I) := ' ';
               else
                  PC_Diff_Img (I - 3 .. I - 1) := "+0x";
                  exit;
               end if;
            end loop;
         end if;

         Put_Token (PC_Diff_Img, Comment);
         Put_Token ("  ", Text);
         if Format = Text then
            Put_Line (Buffer.Get_Raw);

         else
            Cur := Buffer.First;
            loop
               exit when Cur = No_Element;
               Put_Token (Text (Cur), Token (Cur));
               Next (Cur);
            end loop;
            Put_Token ((1 => ASCII.LF), Text);
         end if;

      end Output_Instruction;

   --  Start of processing for Disassemble

   begin
      Open_File (Exec, Exec_File_Name, 0);
      Disassembler := Elf_Disassemblers.Disa_For_Machine (Machine);
      Build_Sections (Exec);
      Build_Symbols (Exec_Acc);
      Build_Debug_Lines (Exec);

      Translate_Locations (Exec_Acc, Locations, Proc_Locs);

      Init_Iterator (Exec, Section_Addresses, Section_Iter);
      Init_Iterator (Exec, Symbol_Addresses, Symbol_Iter);
      Next_Iterator (Symbol_Iter, Symbol);
      Last_Symbol := Symbol;
      Last_Output_Symbol := null;

      loop
         Next_Iterator (Section_Iter, Section);
         exit when Section = null;

         Load_Section_Content (Exec, Section);
         PC := Section.First;
         Insns := Section.Section_Content;

         while PC <= Insns.Last loop

            --  Get the symbol that covers PC

            while Symbol /= null and then Symbol.First <= PC loop
               Last_Symbol := Symbol;
               Next_Iterator (Symbol_Iter, Symbol);
            end loop;

            if Matches_Locations (Exec_Acc, Proc_Locs, PC) then
               Disassembler.Disassemble_Insn
                 (Slice (Insns, PC, Insns.Last), PC, Buffer, Insn_Len, Exec);
               Disassembler.Get_Insn_Properties
                 (Slice (Insns, PC, Insns.Last), PC,
                  Branch,
                  Flag_Indir, Flag_Cond,
                  Branch_Dest, FT_Dest);
               Mnemonic_Kind :=
                 Disassemblers.Get_Mnemonic_Kind
                   (Branch, Flag_Cond);

               Output_Instruction;
               Buffer.Reset;

            else
               Insn_Len := Disassembler.Get_Insn_Length (Insns);
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

      end loop;

      Close_File (Exec);
   end Disassemble;

end Disassemble_Source;
