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
with Interfaces; use Interfaces;
with Ada.Unchecked_Deallocation;

package body Elf_Files is
   function Get_My_Data return Elf_Uchar
   is
      type Arr4 is array (0 .. 3) of Elf_Uchar;
      function To_Arr4 is new Ada.Unchecked_Conversion (Elf_Word, Arr4);
      V : constant Arr4 := To_Arr4 (16#01020304#);
   begin
      if V = (16#01#, 16#02#, 16#03#, 16#04#) then
         return ELFDATA2MSB;
      elsif V = (16#04#, 16#03#, 16#02#, 16#01#) then
         return ELFDATA2LSB;
      else
         return ELFDATANONE;
      end if;
   end Get_My_Data;

   My_Data : constant Elf_Uchar := Get_My_Data;

   procedure Open_File (File : out Elf_File; Filename : String)
   is
      use GNAT.OS_Lib;
   begin
      File := (Filename => new String'(Filename),
               Status => Status_Ok,
               Need_Swap => False,
               Fd => Invalid_FD,
               Ehdr => <>,
               Shdr => null,
               Sh_Strtab => null);

      --  Open the file.
      File.Fd := Open_Read (Filename, Binary);
      if File.Fd = Invalid_FD then
         File.Status := Status_Open_Failure;
         raise Error;
      end if;

      --  Read the Ehdr
      if Read (File.Fd, File.Ehdr'address, Elf_Ehdr_Size) /= Elf_Ehdr_Size then
         File.Status := Status_Read_Error;
         Close (File.Fd);
         File.Fd := Invalid_Fd;
         raise Error;
      end if;

      if File.Ehdr.E_Ident (EI_MAG0) /= ELFMAG0
        or File.Ehdr.E_Ident (EI_MAG1) /= ELFMAG1
        or File.Ehdr.E_Ident (EI_MAG2) /= ELFMAG2
        or File.Ehdr.E_Ident (EI_MAG3) /= ELFMAG3
      then
         File.Status := Status_Bad_Magic;
         Close (File.Fd);
         File.Fd := Invalid_Fd;
         raise Error;
      end if;

      if File.Ehdr.E_Ident (EI_CLASS) /= Elf_Arch_Class
--        or Ehdr.E_Ident (EI_DATA) /= ELFDATA2LSB
        or File.Ehdr.E_Ident (EI_VERSION) /= EV_CURRENT
      then
         File.Status := Status_Bad_Class;
         Close (File.Fd);
         File.Fd := Invalid_Fd;
         raise Error;
      end if;

      File.Need_Swap := File.Ehdr.E_Ident (EI_DATA) /= My_Data;

      if File.Need_Swap then
         Elf_Ehdr_Swap (File.Ehdr);
      end if;

   end Open_File;

   procedure Close_File (File : in out Elf_File)
   is
      use GNAT.OS_Lib;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (String, String_Acc);
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Elf_Shdr_Arr, Elf_Shdr_Arr_Acc);
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Elf_Strtab, Elf_Strtab_Acc);
   begin
      Close (File.Fd);
      File.Fd := Invalid_Fd;
      Unchecked_Deallocation (File.Filename);
      Unchecked_Deallocation (File.Shdr);
      Unchecked_Deallocation (File.Sh_Strtab);
   end Close_File;

   function Get_Status (File : Elf_File) return Elf_File_Status is
   begin
      return File.Status;
   end Get_Status;

   function Get_Filename (File : Elf_File) return String is
   begin
      return File.Filename.all;
   end Get_Filename;

   procedure Load_Shdr (File : in out Elf_File)
   is
      use GNAT.OS_Lib;
      Size : Natural;
   begin
      if Get_Ehdr (File).E_Shentsize /= Elf_Half (Elf_Shdr_Size) then
         raise Error;
      end if;
      if File.Shdr /= null then
         return;
      end if;

      File.Shdr := new Elf_Shdr_Arr (0 .. File.Ehdr.E_Shnum);

      Lseek (File.Fd, Long_Integer (File.Ehdr.E_Shoff), Seek_Set);

      Size := Natural (File.Ehdr.E_Shnum) * Elf_Shdr_Size;

      if Read (File.Fd, File.Shdr (0)'Address, Size) /= Size then
         raise Error;
      end if;

      if File.Need_Swap then
         for I in 1 .. File.Ehdr.E_Shnum loop
            Elf_Shdr_Swap (File.Shdr (I - 1));
         end loop;
      end if;

      File.Sh_Strtab :=
        new Elf_Strtab (0 .. File.Shdr (File.Ehdr.E_Shstrndx).Sh_Size);
      Load_Section (File, File.Ehdr.E_Shstrndx, File.Sh_Strtab (0)'Address);
      -- File.Sh_Strtab := Get_Strtab (File, Get_Ehdr (File).E_Shstrndx);
   end Load_Shdr;

   procedure Load_Section (File : Elf_File; Shdr : Elf_Shdr_Acc; Addr : Address)
   is
      use GNAT.OS_Lib;
      Size : Natural;
   begin
      Lseek (File.Fd, Long_Integer (Shdr.Sh_Offset), Seek_Set);
      Size := Natural (Shdr.Sh_Size);
      if Read (File.Fd, Addr, Size) /= Size then
         raise Error;
      end if;
   end Load_Section;

   procedure Load_Section (File : Elf_File; Index : Elf_Half; Addr : Address)
   is
   begin
      if File.Shdr = null then
         raise Error;
      end if;
      Load_Section (File, Get_Shdr (File, Index), Addr);
   end Load_Section;

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr is
   begin
      return File.Ehdr;
   end Get_Ehdr;

   function Get_Shdr (File : Elf_File; Index : Elf_Half)
                     return Elf_Shdr_Acc
   is
   begin
      if Index >= File.Ehdr.E_Shnum then
         raise Constraint_Error;
      end if;
      return File.Shdr (Index)'Access;
   end Get_Shdr;

   function Get_Shdr_Num (File : Elf_File) return Elf_Half is
   begin
      return File.Ehdr.E_Shnum;
   end Get_Shdr_Num;

--     function Get_Section_Name (File : Elf_File; Index : Elf_Half)
--                               return String is
--     begin
--        raise Error;
--        return "";
--     end Get_Section_Name;

--     function Get_Section_By_Name (File : Elf_File; Name : String)
--                                  return Elf_Half is
--     begin
--        raise Error;
--        return 0;
--     end Get_Section_By_Name;

   function Get_Section_Length (File : Elf_File; Index : Elf_Half)
                                      return Elf_Size is
   begin
      if Index >= File.Ehdr.E_Shnum then
         raise Constraint_Error;
      end if;
      return File.Shdr (Index).Sh_Size;
   end Get_Section_Length;

   --  Load a section in memory.  Only the file length bytes are loaded.

   function Get_String (Strtab : Elf_Strtab_Acc; Idx : Elf_Size) return String
   is
      E : Elf_Size;
   begin
      E := Idx;
      while Strtab (E) /= Nul loop
         E := E + 1;
      end loop;
      if E = Idx then
         return "";
      else
         return String (Strtab (Idx .. E - 1));
      end if;
   end Get_String;


   function Get_Shdr_Name (File : Elf_File; Index : Elf_Half)
                          return String
   is
   begin
      return Get_String (File.Sh_Strtab,
                         Elf_Size (Get_Shdr (File, Index).Sh_Name));
   end Get_Shdr_Name;

   function Get_Shdr_By_Name (File : Elf_File; Name : String)
                                return Elf_Half
   is
      Shdr : Elf_Shdr_Acc;
   begin
      for I in 1 .. File.Ehdr.E_Shnum - 1 loop
         Shdr := Get_Shdr (File, I);
         if Get_String (File.Sh_Strtab, Elf_Size (Shdr.Sh_Name)) = Name then
            return I;
         end if;
      end loop;
      return SHN_UNDEF;
   end Get_Shdr_By_Name;

   function Get_Shdr_By_Name (File : Elf_File; Name : String)
                             return Elf_Shdr_Acc
   is
      I : Elf_Half;
   begin
      I := Get_Shdr_By_Name (File, Name);
      if I = 0 then
         return null;
      else
         return File.Shdr (I)'Access;
      end if;
   end Get_Shdr_By_Name;

   function Get_Sym (File : Elf_File; Addr : Address) return Elf_Sym
   is
      Res : Elf_Sym;
      type Elf_Sym_Acc is access Elf_Sym;
      function To_Elf_Sym_Acc is new Ada.Unchecked_Conversion
        (Address, Elf_Sym_Acc);
   begin
      Res := To_Elf_Sym_Acc (Addr).all;
      if File.Need_Swap then
         Elf_Sym_Swap (Res);
      end if;
      return Res;
   end Get_Sym;

end Elf_Files;
