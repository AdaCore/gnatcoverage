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

package Elf32 is
   subtype Elf32_Addr  is Unsigned_32;
   subtype Elf32_Half  is Unsigned_16;
   subtype Elf32_Off   is Unsigned_32;
   subtype Elf32_Sword is Integer_32;
   subtype Elf32_Word  is Unsigned_32;
   subtype Elf32_Uchar is Unsigned_8;

   type Elf32_Ehdr is record
      E_Ident     : E_Ident_Type;
      E_Type      : Elf32_Half;
      E_Machine   : Elf32_Half;
      E_Version   : Elf32_Word;
      E_Entry     : Elf32_Addr;
      E_Phoff     : Elf32_Off;
      E_Shoff     : Elf32_Off;
      E_Flags     : Elf32_Word;
      E_Ehsize    : Elf32_Half;
      E_Phentsize : Elf32_Half;
      E_Phnum     : Elf32_Half;
      E_Shentsize : Elf32_Half;
      E_Shnum     : Elf32_Half;
      E_Shstrndx  : Elf32_Half;
   end record;

   Elf32_Ehdr_Size : constant Natural := Elf32_Ehdr'Size / System.Storage_Unit;

   procedure Elf32_Ehdr_Swap (Ehdr : in out Elf32_Ehdr);

   type Elf32_Shdr is record
      Sh_Name      : Elf32_Word;
      Sh_Type      : Elf32_Word;
      Sh_Flags     : Elf32_Word;
      Sh_Addr      : Elf32_Addr;
      Sh_Offset    : Elf32_Off;
      Sh_Size      : Elf32_Word;
      Sh_Link      : Elf32_Word;
      Sh_Info      : Elf32_Word;
      Sh_Addralign : Elf32_Word;
      Sh_Entsize   : Elf32_Word;
   end record;
   Elf32_Shdr_Size : constant Natural := Elf32_Shdr'Size / System.Storage_Unit;

   procedure Elf32_Shdr_Swap (Shdr : in out Elf32_Shdr);

   --  Symbol table.
   type Elf32_Sym is record
      St_Name  : Elf32_Word;
      St_Value : Elf32_Addr;
      St_Size  : Elf32_Word;
      St_Info  : Elf32_Uchar;
      St_Other : Elf32_Uchar;
      St_Shndx : Elf32_Half;
   end record;
   Elf32_Sym_Size : constant Natural := Elf32_Sym'Size / System.Storage_Unit;

   procedure Elf32_Sym_Swap (Sym : in out Elf32_Sym);

   function Elf32_St_Bind (Info : Elf32_Uchar) return Elf32_Uchar;
   function Elf32_St_Type (Info : Elf32_Uchar) return Elf32_Uchar;
   function Elf32_St_Info (B, T : Elf32_Uchar) return Elf32_Uchar;
   pragma Inline (Elf32_St_Bind);
   pragma Inline (Elf32_St_Type);
   pragma Inline (Elf32_St_Info);

   --  Relocation.
   type Elf32_Rel is record
      R_Offset : Elf32_Addr;
      R_Info : Elf32_Word;
   end record;
   Elf32_Rel_Size : constant Natural := Elf32_Rel'Size / System.Storage_Unit;

   type Elf32_Rela is record
      R_Offset : Elf32_Addr;
      R_Info : Elf32_Word;
      R_Addend : Elf32_Sword;
   end record;
   Elf32_Rela_Size : constant Natural := Elf32_Rela'Size / System.Storage_Unit;

   procedure Elf32_Rela_Swap (Rela : in out Elf32_Rela);

   function Elf32_R_Sym (I : Elf32_Word) return Elf32_Word;
   function Elf32_R_Type (I : Elf32_Word) return Elf32_Word;
   function Elf32_R_Info (S, T : Elf32_Word) return Elf32_Word;

   type Elf32_Phdr is record
      P_Type   : Elf32_Word;
      P_Offset : Elf32_Off;
      P_Vaddr  : Elf32_Addr;
      P_Paddr  : Elf32_Addr;
      P_Filesz : Elf32_Word;
      P_Memsz  : Elf32_Word;
      P_Flags  : Elf32_Word;
      P_Align  : Elf32_Word;
   end record;
   Elf32_Phdr_Size : constant Natural := Elf32_Phdr'Size / System.Storage_Unit;
end Elf32;
