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
with Interfaces;
with Elf_Common; use Elf_Common;
with Elf32; use Elf32;

package Arch is
   subtype Arch_Addr is Interfaces.Unsigned_32;

   subtype Elf_Ehdr is Elf32_Ehdr;
   subtype Elf_Shdr is Elf32_Shdr;
   subtype Elf_Sym is Elf32_Sym;
   subtype Elf_Rel is Elf32_Rel;
   subtype Elf_Rela is Elf32_Rela;
   subtype Elf_Phdr is Elf32_Phdr;
   subtype Elf_Addr is Elf32_Addr;

   subtype Elf_Off is Elf32_Off;
   Elf_Ehdr_Size : constant Natural := Elf32_Ehdr_Size;
   Elf_Shdr_Size : constant Natural := Elf32_Shdr_Size;
   Elf_Phdr_Size : constant Natural := Elf32_Phdr_Size;
   Elf_Sym_Size  : constant Natural := Elf32_Sym_Size;
   Elf_Rela_Size : constant Natural := Elf32_Rela_Size;

   Elf_Arch_Class : constant Elf_Uchar := ELFCLASS32;

   procedure Elf_Ehdr_Swap (Ehdr : in out Elf_Ehdr)
     renames Elf32_Ehdr_Swap;
   procedure Elf_Shdr_Swap (Shdr : in out Elf_Shdr)
     renames Elf32_Shdr_Swap;
   procedure Elf_Rela_Swap (Rela : in out Elf_Rela)
     renames Elf32_Rela_Swap;
   procedure Elf_Sym_Swap (Sym : in out Elf_Sym)
     renames Elf32_Sym_Swap;

   function Elf_R_Sym (I : Elf_Word) return Elf_Word
     renames Elf32_R_Sym;
   function Elf_R_Type (I : Elf_Word) return Elf_Word
     renames Elf32_R_Type;
   function Elf_R_Info (S, T : Elf_Word) return Elf_Word
     renames Elf32_R_Info;
end Arch;
