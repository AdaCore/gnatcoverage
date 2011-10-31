------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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
with System; use System;
with Elf_Common; use Elf_Common;
with Elf_Arch; use Elf_Arch;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;

package Elf_Files is
   type Elf_File is limited private;

   Error : exception;

   type Elf_File_Status is
     (
      --  No error.
      Status_Ok,

      --  Cannot open file.
      Status_Open_Failure,

      Status_Bad_File,
      Status_Memory,
      Status_Read_Error,
      Status_Bad_Magic,
      Status_Bad_Class
      );

   --  Open a binary file.
   procedure Open_File (File : out Elf_File; Filename : String);

   procedure Close_File (File : in out Elf_File);

   --  Return status of previous operation.
   function Get_Status (File : Elf_File) return Elf_File_Status;

   function Get_Filename (File : Elf_File) return String;

   --  Get ELF header.
   type Elf_Ehdr_Acc is access constant Elf_Ehdr;

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr;

   --  Load internally Section header table and well as shdr string table.
   procedure Load_Shdr (File : in out Elf_File);

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
                               return Elf_Size;

   --  Extract and swap bytes (if necessary) a symbol entry.
   function Get_Sym (File : Elf_File; Addr : Address) return Elf_Sym;

   --  Load a section in memory.  Only the file length bytes are loaded.
   procedure Load_Section (File : Elf_File; Index : Elf_Half; Addr : Address);
   procedure Load_Section (File : Elf_File;
                           Shdr : Elf_Shdr_Acc; Addr : Address);

   type Elf_Strtab is array (Elf_Size range <>) of Character;
   type Elf_Strtab_Acc is access Elf_Strtab;
private
   type Strtab_Fat_Type is array (Elf_Size) of Character;
   type Strtab_Fat_Acc is access all Strtab_Fat_Type;

   type Strtab_Type is record
      Base : Strtab_Fat_Acc;
      Length : Elf_Size;
   end record;

   Null_Strtab : constant Strtab_Type := (null, 0);

   Nul : constant Character := Character'Val (0);

   function To_Strtab_Fat_Acc is new Ada.Unchecked_Conversion
     (Address, Strtab_Fat_Acc);

   type String_Access is access String;

   function To_Elf_Ehdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Ehdr_Acc);

   function To_Elf_Shdr_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Shdr_Acc);

   type Elf_Sym_Acc is access all Elf_Sym;
   function To_Elf_Sym_Acc is new Ada.Unchecked_Conversion
     (Address, Elf_Sym_Acc);

   type Elf_Shdr_Arr is array (Elf_Half range <>) of aliased Elf_Shdr;

   type Elf_Shdr_Arr_Acc is access all Elf_Shdr_Arr;

   type Elf_File is record
      --  Name of the file.
      Filename : String_Access;

      Fd : GNAT.OS_Lib.File_Descriptor;

      --  Status, used to report errors.
      Status : Elf_File_Status;

      Need_Swap : Boolean;

      Ehdr : aliased Elf_Ehdr;

      Shdr : Elf_Shdr_Arr_Acc;

      Sh_Strtab : Elf_Strtab_Acc;
   end record;
end Elf_Files;
