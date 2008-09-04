------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2006 Tristan Gingold                   --
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
with Elf_Common; use Elf_Common;
with Elf32; use Elf32;

package Elf_Arch32 is
   subtype Elf_Ehdr is Elf32_Ehdr;
   subtype Elf_Shdr is Elf32_Shdr;
   subtype Elf_Sym is Elf32_Sym;
   subtype Elf_Rel is Elf32_Rel;
   subtype Elf_Rela is Elf32_Rela;
   subtype Elf_Phdr is Elf32_Phdr;
   subtype Elf_Addr is Elf32_Addr;

   subtype Elf_Off is Elf32_Off;
   subtype Elf_Size is Elf32_Word;
   Elf_Ehdr_Size : constant Natural := Elf32_Ehdr_Size;
   Elf_Shdr_Size : constant Natural := Elf32_Shdr_Size;
   Elf_Phdr_Size : constant Natural := Elf32_Phdr_Size;
   Elf_Sym_Size : constant Natural := Elf32_Sym_Size;

   Elf_Arch_Class : constant Elf_Uchar := ELFCLASS32;

   procedure Elf_Ehdr_Swap (Ehdr : in out Elf_Ehdr)
     renames Elf32_Ehdr_Swap;
   procedure Elf_Shdr_Swap (Ehdr : in out Elf_Shdr)
     renames Elf32_Shdr_Swap;
   procedure Elf_Sym_Swap (Ehdr : in out Elf_Sym)
     renames Elf32_Sym_Swap;
end Elf_Arch32;
