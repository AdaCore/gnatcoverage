------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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
with Ada.Unchecked_Deallocation;
with Elf_Common; use Elf_Common;
with Elf_Arch; use Elf_Arch;
with Elf_Files; use Elf_Files;
with Traces_Elf; use Traces_Elf;
with Traces; use Traces;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;
with Hex_Images; use Hex_Images;
with Dwarf_Handling; use Dwarf_Handling;

package body Traces_Names is
   procedure Disp_Routines_List (Efile : Elf_File)
   is
      use Addresses_Containers;

      Nbr_Shdr : constant Elf_Half := Get_Shdr_Num (Efile);
      type Set_Acc is access Addresses_Containers.Set;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Addresses_Containers.Set, Set_Acc);
      type Set_Acc_Array is array (0 .. Nbr_Shdr) of Set_Acc;
      Shdr_Sets : Set_Acc_Array := (others => null);

      Shdr : Elf_Shdr_Acc;
      Last : Pc_Type;
      Addr : Pc_Type;

      Sym : Addresses_Info_Acc;
      Cur_Sym : Cursor;

      Symtab_Idx : Elf_Half;
      Symtab_Shdr : Elf_Shdr_Acc;
      Symtab_Len : Elf_Size;
      Symtabs : Binary_Content_Acc;

      Strtab_Idx : Elf_Half;
      Strtab_Len : Elf_Size;
      Strtabs : Binary_Content_Acc;

      A_Sym : Elf_Sym;
      Sym_Name : String_Acc;

      Sym_Type : Unsigned_8;
      Cur : Cursor;
      Ok : Boolean;
   begin
      --  Search symtab and strtab.
      --  Exit in case of failure.
      Symtab_Idx := Get_Shdr_By_Name (Efile, ".symtab");
      if Symtab_Idx = SHN_UNDEF then
         Put_Line ("# No symbol table - file stripped ?");
         return;
      end if;
      Symtab_Shdr := Get_Shdr (Efile, Symtab_Idx);
      if Symtab_Shdr.Sh_Type /= SHT_SYMTAB
        or else Symtab_Shdr.Sh_Link = 0
        or else Natural (Symtab_Shdr.Sh_Entsize) /= Elf_Sym_Size
      then
         Put_Line ("# no strtab for .symtab - ill formed ELF file ?");
         return;
      end if;
      Strtab_Idx := Elf_Half (Symtab_Shdr.Sh_Link);

      --  Build sets for A+X sections.
      for Idx in 0 .. Nbr_Shdr - 1 loop
         Shdr := Get_Shdr (Efile, Idx);

         --  Only A+X sections are interesting.
         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
         then
            Shdr_Sets (Idx) := new Addresses_Containers.Set;
         end if;
      end loop;

      --  Load symtab and strtab.
      Symtab_Len := Get_Section_Length (Efile, Symtab_Idx);
      Symtabs := new Binary_Content (0 .. Symtab_Len - 1);
      Load_Section (Efile, Symtab_Idx, Symtabs (0)'Address);

      Strtab_Len := Get_Section_Length (Efile, Strtab_Idx);
      Strtabs := new Binary_Content (0 .. Strtab_Len - 1);
      Load_Section (Efile, Strtab_Idx, Strtabs (0)'Address);

      --  Walk the symtab and put interesting symbols into the containers.
      for I in 1 .. Natural (Symtab_Len) / Elf_Sym_Size loop
         A_Sym := Get_Sym
           (Efile,
            Symtabs (0)'Address + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (A_Sym.St_Info);
         if  (Sym_Type = STT_FUNC or Sym_Type = STT_NOTYPE)
           and then A_Sym.St_Shndx in Shdr_Sets'Range
           and then Shdr_Sets (A_Sym.St_Shndx) /= null
         then
            Sym_Name := new String'
              (Read_String (Strtabs (A_Sym.St_Name)'Address));

            Addresses_Containers.Insert
              (Shdr_Sets (A_Sym.St_Shndx).all,
               new Addresses_Info'
               (Kind => Symbol_Addresses,
                First => Pc_Type (A_Sym.St_Value),
                Last => Pc_Type (A_Sym.St_Value + A_Sym.St_Size - 1),
                Parent => null,
                Symbol_Name => Sym_Name),
               Cur, Ok);
            if not Ok then
               Put_Line (Standard_Error,
                         "symbol " & Sym_Name.all
                           & " is an alias at " & Hex_Image (A_Sym.St_Value));
            end if;
         end if;
      end loop;
      Unchecked_Deallocation (Strtabs);
      Unchecked_Deallocation (Symtabs);

      --  Walk the sections and display the routines.
      for I in Shdr_Sets'Range loop
         if Shdr_Sets (I) /= null then
            Shdr := Get_Shdr (Efile, I);

            Addr := Pc_Type (Shdr.Sh_Addr);
            Last := Pc_Type (Shdr.Sh_Addr + Shdr.Sh_Size - 1);

            Put_Line ("# " & Hex_Image (Addr) & "-" & Hex_Image (Last)
                        & ": " & Get_Shdr_Name (Efile, I));

            Cur_Sym := First (Shdr_Sets (I).all);
            if Cur_Sym /= No_Element then
               Sym := Element (Cur_Sym);
            else
               Sym := null;
            end if;

            --  Get the first symbol in the section.
            while Sym /= null and then Sym.First < Addr loop
               Next (Cur_Sym);
               if Cur_Sym = No_Element then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            while Sym /= null and then Sym.Last <= Last loop
               if Sym.First > Sym.Last then
                  if Sym.First <= Last then
                     Put_Line
                       (Standard_Error, "empty symbol " & Sym.Symbol_Name.all
                          & " at " & Hex_Image (Sym.First));
                  end if;
               else
                  if Sym.First > Addr then
                     Put_Line
                       (Standard_Error, "no symbols for "
                          & Hex_Image (Addr) & "-" & Hex_Image (Sym.First - 1));
                  end if;
                  Put_Line (Sym.Symbol_Name.all);
                  Addr := Sym.Last;
                  exit when Addr = Pc_Type'Last;
                  Addr := Addr + 1;
               end if;

               Next (Cur_Sym);
               if Cur_Sym = No_Element then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            if Addr < Last then
               Put_Line
                 (Standard_Error, "no symbols for "
                    & Hex_Image (Addr) & "-" & Hex_Image (Last));
            end if;
            Unchecked_Deallocation (Shdr_Sets (I));
         end if;
      end loop;
   end Disp_Routines_List;

   procedure Disp_Routines_List (Filename : String)
   is
      Efile : Elf_File;
   begin
      Open_File (Efile, Filename);
      Load_Shdr (Efile);
      Disp_Routines_List (Efile);
      Close_File (Efile);
   end Disp_Routines_List;

end Traces_Names;
