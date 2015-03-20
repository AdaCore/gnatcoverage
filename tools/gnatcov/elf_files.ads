------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Ada.Unchecked_Conversion;

with System; use System;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.Mmap; use GNATCOLL.Mmap;
with Binary_Files;  use Binary_Files;

with Elf_Common; use Elf_Common;
with Arch; use Arch;

package Elf_Files is
   type Elf_File is new Binary_File with private;

   --  Open a binary file.
   function Create_File
     (Fd : File_Descriptor; Filename : String_Access) return Elf_File;

   --  Check if the file is an ELF file.
   function Is_ELF_File (Fd : File_Descriptor) return Boolean;

   type Elf_Ehdr_Acc is access constant Elf_Ehdr;

   --  Get ELF header.
   function Get_Ehdr (File : Elf_File) return Elf_Ehdr;

   procedure Load_Shdr (File : in out Elf_File);
   --  Load internally Section header table and well as shdr string table.

   procedure Enable_Section_Relocation (File : in out Elf_File);
   --  Reload the Section header table if needed to make it mutable. This will
   --  enable one to relocate sections. The Section header table must already
   --  be loaded.

   type Elf_Shdr_Acc is access all Elf_Shdr;

   function Get_Shdr_Num (File : Elf_File) return Elf_Half;

   --  Get a Shdr.
   function Get_Shdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Shdr_Acc;

   function Get_Shdr_Name (File : Elf_File; Index : Elf_Half)
                          return String;

   function Get_Shdr_By_Name (File : Elf_File; Name : String)
                             return Elf_Half;
   function Get_Shdr_By_Name (File : Elf_File; Name : String)
                             return Elf_Shdr_Acc;

   function Get_Section_Length (File : Elf_File; Index : Elf_Half)
                               return Elf_Addr;

   --  Extract and swap bytes (if necessary) a relocation entry
   function Get_Rela (File : Elf_File; Addr : Address) return Elf_Rela;

   --  Extract and swap bytes (if necessary) a symbol entry.
   function Get_Sym (File : Elf_File; Addr : Address) return Elf_Sym;

   function Load_Section
     (File : Elf_File; Index : Elf_Half) return Mapped_Region;
   function Load_Section
     (File : Elf_File; Shdr : Elf_Shdr_Acc) return Mapped_Region;
   --  Load a section in memory. Only the file length bytes are loaded

   procedure Make_Mutable
     (File : Elf_File; Region : in out Mapped_Region);
   --  Make some previously-mapped region mutable

   type Elf_Rela_Acc is access Elf_Rela;

   type Elf_Strtab is array (Elf_Addr) of Character;
   type Elf_Strtab_Acc is access Elf_Strtab;

private
   type Strtab_Fat_Type is array (Elf_Addr) of Character;
   type Strtab_Fat_Acc is access all Strtab_Fat_Type;

   type Elf_Ehdr_Var_Acc is access all Elf_Ehdr;

   type Strtab_Type is record
      Base : Strtab_Fat_Acc;
      Length : Elf_Addr;
   end record;

   Null_Strtab : constant Strtab_Type := (null, 0);

   Nul : constant Character := Character'Val (0);

   type Elf_Shdr_Arr is array (Elf_Half) of aliased Elf_Shdr;
   type Elf_Shdr_Arr_Acc is access all Elf_Shdr_Arr;

   function To_Elf_Shdr_Arr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Shdr_Arr_Acc);

   function To_Elf_Strtab_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Strtab_Acc);

   function To_Elf_Ehdr_Var_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Ehdr_Var_Acc);

   function To_Elf_Rela_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Rela_Acc);

   function To_Strtab_Fat_Acc is new Ada.Unchecked_Conversion
     (Address, Strtab_Fat_Acc);

   function To_Elf_Ehdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Ehdr_Acc);

   function To_Elf_Shdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Shdr_Acc);

   type Elf_Sym_Acc is access all Elf_Sym;
   function To_Elf_Sym_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Sym_Acc);

   type Elf_File is new Binary_File with record
      Need_Swap        : Boolean;
      --  Whether the endianess of the ELF is the same as the one on this
      --  machine.

      Ehdr_Map         : Mapped_Region;
      Shdr_Map         : Mapped_Region;
      Sh_Strtab_Map    : Mapped_Region;

      Ehdr             : Elf_Ehdr_Var_Acc;
      Shdr             : Elf_Shdr_Arr_Acc;
      Sh_Strtab        : Elf_Strtab_Acc;
   end record;
end Elf_Files;
