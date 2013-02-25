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
with Interfaces; use Interfaces;
with System;
with Elf_Common; use Elf_Common;

package Elf64 is
   subtype Elf64_Addr  is Unsigned_64;
   subtype Elf64_Half  is Unsigned_16;
   subtype Elf64_Off   is Unsigned_64;
   subtype Elf64_Sxword  is Integer_64;
   subtype Elf64_Xword  is Unsigned_64;
   subtype Elf64_Sword is Integer_32;
   subtype Elf64_Word  is Unsigned_32;
   subtype Elf64_Uchar is Unsigned_8;

   type Elf64_Ehdr is record
      E_Ident     : E_Ident_Type;
      E_Type      : Elf64_Half;
      E_Machine   : Elf64_Half;
      E_Version   : Elf64_Word;
      E_Entry     : Elf64_Addr;
      E_Phoff     : Elf64_Off;
      E_Shoff     : Elf64_Off;
      E_Flags     : Elf64_Word;
      E_Ehsize    : Elf64_Half;
      E_Phentsize : Elf64_Half;
      E_Phnum     : Elf64_Half;
      E_Shentsize : Elf64_Half;
      E_Shnum     : Elf64_Half;
      E_Shstrndx  : Elf64_Half;
   end record;

   Elf64_Ehdr_Size : constant Natural := Elf64_Ehdr'Size / System.Storage_Unit;

   procedure Elf64_Ehdr_Swap (Ehdr : in out Elf64_Ehdr);

   type Elf64_Shdr is record
      Sh_Name      : Elf64_Word;
      Sh_Type      : Elf64_Word;
      Sh_Flags     : Elf64_Xword;
      Sh_Addr      : Elf64_Addr;
      Sh_Offset    : Elf64_Off;
      Sh_Size      : Elf64_Xword;
      Sh_Link      : Elf64_Word;
      Sh_Info      : Elf64_Word;
      Sh_Addralign : Elf64_Xword;
      Sh_Entsize   : Elf64_Xword;
   end record;
   Elf64_Shdr_Size : constant Natural := Elf64_Shdr'Size / System.Storage_Unit;

   procedure Elf64_Shdr_Swap (Shdr : in out Elf64_Shdr);

   --  Symbol table.
   type Elf64_Sym is record
      St_Name  : Elf64_Word;
      St_Info  : Elf64_Uchar;
      St_Other : Elf64_Uchar;
      St_Shndx : Elf64_Half;
      St_Value : Elf64_Addr;
      St_Size  : Elf64_Xword;
   end record;
   Elf64_Sym_Size : constant Natural := Elf64_Sym'Size / System.Storage_Unit;

   procedure Elf64_Sym_Swap (Sym : in out Elf64_Sym);

   function Elf64_St_Bind (Info : Elf64_Uchar) return Elf64_Uchar;
   function Elf64_St_Type (Info : Elf64_Uchar) return Elf64_Uchar;
   function Elf64_St_Info (B, T : Elf64_Uchar) return Elf64_Uchar;
   pragma Inline (Elf64_St_Bind);
   pragma Inline (Elf64_St_Type);
   pragma Inline (Elf64_St_Info);

   --  Relocation.
   type Elf64_Rel is record
      R_Offset : Elf64_Addr;
      R_Info : Elf64_Xword;
   end record;
   Elf64_Rel_Size : constant Natural := Elf64_Rel'Size / System.Storage_Unit;

   type Elf64_Rela is record
      R_Offset : Elf64_Addr;
      R_Info : Elf64_Xword;
      R_Addend : Elf64_Sxword;
   end record;
   Elf64_Rela_Size : constant Natural := Elf64_Rela'Size / System.Storage_Unit;

   procedure Elf64_Rela_Swap (Rela : in out Elf64_Rela);

   function Elf64_R_Sym (I : Elf64_Xword) return Elf64_Word;
   function Elf64_R_Type (I : Elf64_Xword) return Elf64_Word;
   function Elf64_R_Info (S, T : Elf64_Word) return Elf64_Xword;

   type Elf64_Phdr is record
      P_Type   : Elf64_Word;
      P_Offset : Elf64_Off;
      P_Vaddr  : Elf64_Addr;
      P_Paddr  : Elf64_Addr;
      P_Filesz : Elf64_Word;
      P_Memsz  : Elf64_Word;
      P_Flags  : Elf64_Word;
      P_Align  : Elf64_Word;
   end record;
   Elf64_Phdr_Size : constant Natural := Elf64_Phdr'Size / System.Storage_Unit;
end Elf64;
