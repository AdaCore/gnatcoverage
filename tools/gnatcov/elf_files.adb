------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;

with GNAT.CRC32; use GNAT.CRC32;

with Strings; use Strings;

package body Elf_Files is

   function Compute_CRC32 (File : Elf_File) return Unsigned_32;
   --  Compute and return the CRC32 of File

   function Get_My_Data return Elf_Uchar;
   function Get_String (Strtab : Elf_Strtab_Acc; Idx : Elf_Addr) return String;

   -----------------
   -- Get_My_Data --
   -----------------

   function Get_My_Data return Elf_Uchar is
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

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File (File : out Elf_File; Filename : String) is
      Basename : constant String := Ada.Directories.Simple_Name (Filename);
   begin
      File := (Filename   => new String'(Filename),
               Status     => Status_Ok,
               Need_Swap  => False,
               Fd         => Invalid_FD,
               Size       => 0,
               Time_Stamp => Invalid_Time,
               CRC32      => 0,
               Ehdr       => <>,
               Shdr       => null,
               Sh_Strtab  => null);

      --  Open the file

      loop
         File.Fd := Open_Read (File.Filename.all, Binary);
         exit when File.Fd /= Invalid_FD;

         --  If open failed and Filename includes a directory name, try again
         --  with just the base name, else bail out.

         if File.Filename.all /= Basename then
            Free (File.Filename);
            File.Filename := new String'(Basename);
         else
            File.Status := Status_Open_Failure;
            raise Error with File.Filename.all & ": not found";
         end if;
      end loop;

      File.Size := File_Length (File.Fd);
      File.Time_Stamp := File_Time_Stamp (File.Fd);

      --  Read the Ehdr

      if Read (File.Fd, File.Ehdr'Address, Elf_Ehdr_Size) /= Elf_Ehdr_Size then
         File.Status := Status_Read_Error;
         Close (File.Fd);
         File.Fd := Invalid_FD;
         raise Error with File.Filename.all & ": failed to read ELF header";
      end if;

      if File.Ehdr.E_Ident (EI_MAG0) /= ELFMAG0
        or else File.Ehdr.E_Ident (EI_MAG1) /= ELFMAG1
        or else File.Ehdr.E_Ident (EI_MAG2) /= ELFMAG2
        or else File.Ehdr.E_Ident (EI_MAG3) /= ELFMAG3
      then
         File.Status := Status_Bad_Magic;
         Close (File.Fd);
         File.Fd := Invalid_FD;

         --  Specialize error message for the case where the user passed a
         --  trace file instead of an ELF file.

         if Has_Suffix (File.Filename.all, ".trace") then
            --  Should we instead get the executable name from the trace file
            --  and retry???

            raise Error with
              File.Filename.all & ": ELF file expected, found a trace file";

         else
            raise Error with File.Filename.all & ": bad ELF magic";
         end if;
      end if;

      if File.Ehdr.E_Ident (EI_CLASS) /= Elf_Arch_Class
        or File.Ehdr.E_Ident (EI_VERSION) /= EV_CURRENT
      then
         File.Status := Status_Bad_Class;
         Close (File.Fd);
         File.Fd := Invalid_FD;
         raise Error with "unexpected ELF class or version";
      end if;

      File.Need_Swap := File.Ehdr.E_Ident (EI_DATA) /= My_Data;

      if File.Need_Swap then
         Elf_Ehdr_Swap (File.Ehdr);
      end if;

      File.CRC32 := Compute_CRC32 (File);
   end Open_File;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (File : in out Elf_File) is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Elf_Shdr_Arr, Elf_Shdr_Arr_Acc);
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Elf_Strtab, Elf_Strtab_Acc);
   begin
      Close (File.Fd);
      File.Fd := Invalid_FD;
      Unchecked_Deallocation (File.Shdr);
      Unchecked_Deallocation (File.Sh_Strtab);

      --  Note: File.Filename may be referenced later on to produce error
      --  messages, so we don't deallocate it.
   end Close_File;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (File : Elf_File) return Elf_File_Status is
   begin
      return File.Status;
   end Get_Status;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (File : Elf_File) return String is
   begin
      return File.Filename.all;
   end Get_Filename;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (File : Elf_File) return Long_Integer is
   begin
      return File.Size;
   end Get_Size;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (File : Elf_File) return OS_Time is
   begin
      return File.Time_Stamp;
   end Get_Time_Stamp;

   ---------------
   -- Get_CRC32 --
   ---------------

   function Get_CRC32 (File : Elf_File) return Unsigned_32 is
   begin
      return File.CRC32;
   end Get_CRC32;

   ---------------
   -- Load_Shdr --
   ---------------

   procedure Load_Shdr (File : in out Elf_File) is
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
   end Load_Shdr;

   ------------------
   -- Load_Section --
   ------------------

   procedure Load_Section
     (File : Elf_File;
      Shdr : Elf_Shdr_Acc;
      Addr : Address)
   is
      Size : Natural;

   begin
      Lseek (File.Fd, Long_Integer (Shdr.Sh_Offset), Seek_Set);
      Size := Natural (Shdr.Sh_Size);
      if Read (File.Fd, Addr, Size) /= Size then
         raise Error;
      end if;
   end Load_Section;

   ------------------
   -- Load_Section --
   ------------------

   procedure Load_Section
     (File  : Elf_File;
      Index : Elf_Half;
      Addr  : Address)
   is
   begin
      if File.Shdr = null then
         raise Error;
      end if;
      Load_Section (File, Get_Shdr (File, Index), Addr);
   end Load_Section;

   --------------
   -- Get_Ehdr --
   --------------

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr is
   begin
      return File.Ehdr;
   end Get_Ehdr;

   --------------
   -- Get_Shdr --
   --------------

   function Get_Shdr (File : Elf_File; Index : Elf_Half) return Elf_Shdr_Acc is
   begin
      if Index >= File.Ehdr.E_Shnum then
         raise Constraint_Error;
      end if;
      return File.Shdr (Index)'Access;
   end Get_Shdr;

   ------------------
   -- Get_Shdr_Num --
   ------------------

   function Get_Shdr_Num (File : Elf_File) return Elf_Half is
   begin
      return File.Ehdr.E_Shnum;
   end Get_Shdr_Num;

   ------------------------
   -- Get_Section_Length --
   ------------------------

   function Get_Section_Length (File : Elf_File; Index : Elf_Half)
                                      return Elf_Addr is
   begin
      if Index >= File.Ehdr.E_Shnum then
         raise Constraint_Error;
      end if;
      return File.Shdr (Index).Sh_Size;
   end Get_Section_Length;

   --  Load a section in memory.  Only the file length bytes are loaded
   --  what is this comment referring to???

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Strtab : Elf_Strtab_Acc; Idx : Elf_Addr) return String
   is
      E : Elf_Addr;
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

   -------------------
   -- Get_Shdr_Name --
   -------------------

   function Get_Shdr_Name
     (File : Elf_File; Index : Elf_Half) return String
   is
   begin
      return Get_String (File.Sh_Strtab,
                         Elf_Addr (Get_Shdr (File, Index).Sh_Name));
   end Get_Shdr_Name;

   ----------------------
   -- Get_Shdr_By_Name --
   ----------------------

   function Get_Shdr_By_Name
     (File : Elf_File; Name : String) return Elf_Half
   is
      Shdr : Elf_Shdr_Acc;
   begin
      for I in 1 .. File.Ehdr.E_Shnum - 1 loop
         Shdr := Get_Shdr (File, I);
         if Get_String (File.Sh_Strtab, Elf_Addr (Shdr.Sh_Name)) = Name then
            return I;
         end if;
      end loop;
      return SHN_UNDEF;
   end Get_Shdr_By_Name;

   ----------------------
   -- Get_Shdr_By_Name --
   ----------------------

   function Get_Shdr_By_Name
     (File : Elf_File; Name : String) return Elf_Shdr_Acc
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

   --------------
   -- Get_Rela --
   --------------

   function Get_Rela (File : Elf_File; Addr : Address) return Elf_Rela is
      Res : Elf_Rela;
   begin
      Res := To_Elf_Rela_Acc (Addr).all;
      if File.Need_Swap then
         Elf_Rela_Swap (Res);
      end if;
      return Res;
   end Get_Rela;

   -------------
   -- Get_Sym --
   -------------

   function Get_Sym (File : Elf_File; Addr : Address) return Elf_Sym is
      Res : Elf_Sym;
   begin
      Res := To_Elf_Sym_Acc (Addr).all;
      if File.Need_Swap then
         Elf_Sym_Swap (Res);
      end if;
      return Res;
   end Get_Sym;

   -------------------
   -- Compute_CRC32 --
   -------------------

   function Compute_CRC32 (File : Elf_File) return Unsigned_32 is
      C      : CRC32;
      Buffer : String (Integer range 1 .. 2 ** 12);
      Size   : Integer;
   begin
      Initialize (C);
      loop
         Size := Read (File.Fd, Buffer'Address, Buffer'Length);
         exit when Size < 1;
         Update (C, Buffer (1 .. Size));
      end loop;

      Lseek (File.Fd, 0, 0);
      return Get_Value (C);
   end Compute_CRC32;

end Elf_Files;
