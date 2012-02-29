------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Elf_Files;  use Elf_Files;
with Elf_Arch32; use Elf_Arch32;
with Elf_Common; use Elf_Common;
with Interfaces; use Interfaces;
with Swaps;      use Swaps;

with Text_IO;     use Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

separate (Gdbtrace)
package body Exec_Info is

   procedure Process_Exec
     (Exe_Filename : String;
      CBA_Ptr      : out Code_Block_Array_Ptr_T;
      Exit_Addr    : out Unsigned_32)
   is
      File_Data : Elf_File;

      --  Getting instructions (creating code blocks) from .text section.
      --  Should look for all sections marked as containing executable
      --  instructions ???

      Text_Shdr_Idx    : Elf_Half;
      Text_Shdr_Ptr    : Elf_Shdr_Acc;
      Text_Section_Len : Elf_Size;
      Text_First_Addr  : Unsigned_32;
      type Insns_Array is array (Positive range <>) of Unsigned_32;
      Insns_Ptr        : access Insns_Array;
      N_Insns          : Positive;

      Strtab_Shdr_Idx   : Elf_Half;
      Strtab_Len        : Elf_Size;
      Strtab_Ptr        : Elf_Strtab_Acc;
      Ending_Symbol_Str : constant String := "abort";
      --  Make string above into parameter ???

      Symtab_Shdr_Idx : Elf_Half;
      Symtab_Len      : Elf_Size;
      type Sym_Array is array (Elf_Size range <>) of Elf_Sym;
      Sym_Array_Ptr : access Sym_Array;
      N_Syms          : Elf_Size;

      Strtab_Idx       : Elf_Size;
      End_Symbol_Found : Boolean;
      End_Symbol_Idx   : Elf_Size;

      Op_Code        : Unsigned_32;
      N_Blocks       : Natural;
      First_Insn_Idx : Positive;
      Block_N        : Positive;
      Block_Start    : Unsigned_32;

   begin

      Open_File (File_Data, Exe_Filename);
      if Get_Status (File_Data) /= Status_Ok then
         raise Program_Error
           with "Error opening executable : " & Exe_Filename & '.';
      end if;
      Load_Shdr (File_Data);
      --  Opening file and retrieving the section header table are
      --  necessary steps for subsequent operations.

      Text_Shdr_Idx    := Get_Shdr_By_Name (File_Data, ".text");
      Text_Shdr_Ptr    := Get_Shdr (File_Data, Text_Shdr_Idx);
      Text_First_Addr  := Text_Shdr_Ptr.Sh_Addr;
      Text_Section_Len := Text_Shdr_Ptr.Sh_Size;
      if Text_Section_Len < 4 then
         raise Program_Error with ".text section too small.";
      end if;
      --  Load .text into an array of insns.
      --  Swap is unconditional still (to be fixed) ???

      --  Need to find all sections containing code ???

      N_Insns := Positive (Text_Section_Len / 4);
      Insns_Ptr := new Insns_Array (1 .. N_Insns);
      Load_Section (File_Data, Text_Shdr_Idx,
                    Insns_Ptr (Insns_Ptr'First)'Address);
      for J in Insns_Ptr'Range loop
         Insns_Ptr (J) := Swap (Insns_Ptr (J));
      end loop;

      N_Blocks := 0;
      for J in Insns_Ptr'Range loop
         Op_Code := Shift_Right (Insns_Ptr (J), 26);
         if Op_Code = 18 or else Op_Code = 16 or else Op_Code = 19 then
            N_Blocks := N_Blocks + 1;
         elsif J = Insns_Ptr'Last then
            N_Blocks := N_Blocks + 1;
         end if;
      end loop;
      --  Make a pass over instructions to count the number of blocks.

      if N_Blocks = 0 then
         raise Program_Error with "No code blocks found.";
      end if;

      CBA_Ptr := new Code_Block_Array_T (1 .. N_Blocks);
      Block_N := 1;
      Block_Start := Text_First_Addr;
      First_Insn_Idx := Insns_Ptr'First;
      CBA_Ptr (Block_N).Start := Block_Start;
      for J in Insns_Ptr'Range loop
         Op_Code := Shift_Right (Insns_Ptr (J), 26);
         if Op_Code = 18 or else Op_Code = 16 or else Op_Code = 19 then
            N_Insns := J - First_Insn_Idx + 1;
            CBA_Ptr (Block_N).N_Insns := N_Insns;
            exit when J = Insns_Ptr'Last;
            Block_N := Block_N + 1;
            Block_Start := Block_Start + Unsigned_32 (N_Insns * 4);
            First_Insn_Idx := J + 1;
            CBA_Ptr (Block_N).Start := Block_Start;
         elsif J = Insns_Ptr'Last then
            N_Insns := J - First_Insn_Idx + 1;
            CBA_Ptr (Block_N).N_Insns := N_Insns;
            exit;
         end if;
      end loop;
      --  Allocate and populate CBA;

      Strtab_Shdr_Idx := Get_Shdr_By_Name (File_Data, ".strtab");
      Strtab_Len := Get_Section_Length (File_Data, Strtab_Shdr_Idx);
      Strtab_Ptr :=
        new Elf_Strtab (Elf_Size'First .. Elf_Size'First + Strtab_Len - 1);
      Load_Section (File_Data, Strtab_Shdr_Idx,
                    Strtab_Ptr (Elf_Size'First)'Address);
      Symtab_Shdr_Idx := Get_Shdr_By_Name (File_Data, ".symtab");
      Symtab_Len := Get_Section_Length (File_Data, Symtab_Shdr_Idx);
      N_Syms := Symtab_Len / 16;
      Sym_Array_Ptr :=
        new Sym_Array (Elf_Size'First .. Elf_Size'First + N_Syms - 1);
      Load_Section (File_Data, Symtab_Shdr_Idx,
                    Sym_Array_Ptr (Elf_Size'First)'Address);
      for J in Elf_Size'First .. Elf_Size'First + N_Syms - 1 loop
         Sym_Array_Ptr (J) := Get_Sym (File_Data, Sym_Array_Ptr (J)'Address);
      end loop;
      --  Need the strings table and symbol table to get the
      --  address of the symbol which indicates completion.

      End_Symbol_Found := False;
      for J in Sym_Array_Ptr'Range loop
         Strtab_Idx := Sym_Array_Ptr (J).St_Name;
         if Strtab_Idx /= 0 then
            for K in Ending_Symbol_Str'Range loop
               exit when Strtab_Idx > Strtab_Ptr'Last;
               exit when Strtab_Ptr (Strtab_Idx) /= Ending_Symbol_Str (K);
               if K = Ending_Symbol_Str'Last then
                  Strtab_Idx := Strtab_Idx + 1;
                  if Strtab_Idx <= Strtab_Ptr'Last
                    and then Strtab_Ptr (Strtab_Idx) = ASCII.NUL
                  then
                     End_Symbol_Found := True;
                     End_Symbol_Idx := J;
                  end if;
                  exit;
               end if;
               Strtab_Idx := Strtab_Idx + 1;
            end loop;
         end if;
      end loop;
      if not End_Symbol_Found then
         raise Program_Error with "End symbol not found.";
      end if;
      --  Find the end symbol's address.

      Exit_Addr := Sym_Array_Ptr (End_Symbol_Idx).St_Value;

   end Process_Exec;
end Exec_Info;
