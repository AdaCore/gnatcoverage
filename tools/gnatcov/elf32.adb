------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
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
with Swaps; use Swaps;

package body Elf32 is

   procedure Elf32_Ehdr_Swap (Ehdr : in out Elf32_Ehdr) is
   begin
      Ehdr := (E_Ident => Ehdr.E_Ident,
               E_Type => Swap (Ehdr.E_Type),
               E_Machine => Swap (Ehdr.E_Machine),
               E_Version => Swap (Ehdr.E_Version),
               E_Entry => Swap (Ehdr.E_Entry),
               E_Phoff => Swap (Ehdr.E_Phoff),
               E_Shoff => Swap (Ehdr.E_Shoff),
               E_Flags => Swap (Ehdr.E_Flags),
               E_Ehsize => Swap (Ehdr.E_Ehsize),
               E_Phentsize => Swap (Ehdr.E_Phentsize),
               E_Phnum => Swap (Ehdr.E_Phnum),
               E_Shentsize => Swap (Ehdr.E_Shentsize),
               E_Shnum => Swap (Ehdr.E_Shnum),
               E_Shstrndx => Swap (Ehdr.E_Shstrndx));
   end Elf32_Ehdr_Swap;

   procedure Elf32_Shdr_Swap (Shdr : in out Elf32_Shdr) is
   begin
      Shdr := (Sh_Name => Swap (Shdr.Sh_Name),
               Sh_Type => Swap (Shdr.Sh_Type),
               Sh_Flags => Swap (Shdr.Sh_Flags),
               Sh_Addr => Swap (Shdr.Sh_Addr),
               Sh_Offset => Swap (Shdr.Sh_Offset),
               Sh_Size => Swap (Shdr.Sh_Size),
               Sh_Link => Swap (Shdr.Sh_Link),
               Sh_Info => Swap (Shdr.Sh_Info),
               Sh_Addralign => Swap (Shdr.Sh_Addralign),
               Sh_Entsize => Swap (Shdr.Sh_Entsize));
   end Elf32_Shdr_Swap;

   procedure Elf32_Chdr_Swap (Chdr : in out Elf32_Chdr) is
   begin
      Chdr := (Ch_Type      => Swap (Chdr.Ch_Type),
               Ch_Size      => Swap (Chdr.Ch_Size),
               Ch_Addralign => Swap (Chdr.Ch_Addralign));
   end Elf32_Chdr_Swap;

   procedure Elf32_Sym_Swap (Sym : in out Elf32_Sym) is
   begin
      Sym := (St_Name => Swap (Sym.St_Name),
              St_Value => Swap (Sym.St_Value),
              St_Size => Swap (Sym.St_Size),
              St_Info => Sym.St_Info,
              St_Other => Sym.St_Other,
              St_Shndx => Swap (Sym.St_Shndx));
   end Elf32_Sym_Swap;

   procedure Elf32_Rela_Swap (Rela : in out Elf32_Rela) is
   begin
      Rela := (R_Offset => Swap (Rela.R_Offset),
               R_Info   => Swap (Rela.R_Info),
               R_Addend => Swap (Rela.R_Addend));
   end Elf32_Rela_Swap;

   function Elf32_St_Bind (Info : Elf32_Uchar) return Elf32_Uchar is
   begin
      return Shift_Right (Info, 4);
   end Elf32_St_Bind;

   function Elf32_St_Type (Info : Elf32_Uchar) return Elf32_Uchar is
   begin
      return Info and 16#0F#;
   end Elf32_St_Type;

   function Elf32_St_Info (B, T : Elf32_Uchar) return Elf32_Uchar is
   begin
      return Shift_Left (B, 4) or T;
   end Elf32_St_Info;

   function Elf32_R_Sym (I : Elf32_Word) return Elf32_Word is
   begin
      return Shift_Right (I, 8);
   end Elf32_R_Sym;

   function Elf32_R_Type (I : Elf32_Word) return Elf32_Word is
   begin
      return I and 16#Ff#;
   end Elf32_R_Type;

   function Elf32_R_Info (S, T : Elf32_Word) return Elf32_Word is
   begin
      return Shift_Left (S, 8) or T;
   end Elf32_R_Info;
end Elf32;
