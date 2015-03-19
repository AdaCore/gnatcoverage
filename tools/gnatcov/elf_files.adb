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

   ---------------------
   -- Open_File_By_Fd --
   ---------------------

   procedure Open_File_By_Fd
     (File : out Elf_File; Fd : File_Descriptor; Filename : String_Access)
   is
      procedure Exit_With_Error (Status : Elf_File_Status; Msg : String);
      --  Assign Status to File, close the file if needed and raise Error with
      --  the filename and Msg.

      ---------------------
      -- Exit_With_Error --
      ---------------------

      procedure Exit_With_Error (Status : Elf_File_Status; Msg : String) is
      begin
         File.Status := Status;
         if File.Fd /= Invalid_FD then
            Close (File.File);
            File.Fd := Invalid_FD;
         end if;
         raise Error with File.Filename.all & ": " & Msg;
      end Exit_With_Error;

   begin
      File := (Filename         => Filename,
               Fd               => Fd,
               File             => Invalid_Mapped_File,
               Status           => Status_Ok,
               Need_Swap        => False,
               Size             => 0,
               Time_Stamp       => Invalid_Time,
               CRC32            => 0,
               Ehdr_Map         => Invalid_Mapped_Region,
               Shdr_Map         => Invalid_Mapped_Region,
               Sh_Strtab_Map    => Invalid_Mapped_Region,
               Ehdr             => null,
               Shdr             => null,
               Sh_Strtab        => null);

      File.File := Open_Read (File.Filename.all);

      File.Size := File_Length (File.Fd);
      File.Time_Stamp := File_Time_Stamp (File.Fd);

      --  Read the Ehdr
      File.Ehdr_Map := Read
        (File.File, 0, File_Size (Elf_Ehdr_Size));
      if Natural (GNATCOLL.Mmap.Last (File.Ehdr_Map)) /= Elf_Ehdr_Size then
         Exit_With_Error (Status_Read_Error, "failed to read ELF header");
      end if;

      --  Make it mutable if byte-swapping is needed

      File.Ehdr := To_Elf_Ehdr_Var_Acc (Data (File.Ehdr_Map).all'Address);
      File.Need_Swap := File.Ehdr.E_Ident (EI_DATA) /= My_Data;

      if File.Need_Swap then
         Make_Mutable (File, File.Ehdr_Map);
         File.Ehdr := To_Elf_Ehdr_Var_Acc (Data (File.Ehdr_Map).all'Address);
      end if;

      if File.Need_Swap then
         Elf_Ehdr_Swap (File.Ehdr.all);
      end if;

      if File.Ehdr.E_Ident (EI_MAG0) /= ELFMAG0
        or else File.Ehdr.E_Ident (EI_MAG1) /= ELFMAG1
        or else File.Ehdr.E_Ident (EI_MAG2) /= ELFMAG2
        or else File.Ehdr.E_Ident (EI_MAG3) /= ELFMAG3
      then
         --  In the case this is a trace file, should we instead get the
         --  executable name from the trace file and retry???

         Exit_With_Error
           (Status_Bad_Magic,
            (if Has_Suffix (File.Filename.all, ".trace")
             then "ELF file expected, found a trace file"
             else "bad ELF magic"));
      end if;

      declare
         Input_Class : Elf_Uchar renames File.Ehdr.E_Ident (EI_CLASS);
      begin
         if Input_Class /= Elf_Arch_Class then
            Exit_With_Error
              (Status_Bad_Class,
               (case Input_Class is
                   when ELFCLASS32 => "unsupported ELF class (32bit)",
                   when ELFCLASS64 => "unsupported ELF class (64bit)",
                   when others =>
                      ("invalid ELF class ("
                       & Elf_Uchar'Image (Input_Class)
                       & ')')));
         end if;
      end;

      if File.Ehdr.E_Ident (EI_VERSION) /= EV_CURRENT then
         Exit_With_Error (Status_Bad_Version, "unexpected ELF version");
      end if;

      File.CRC32 := Compute_CRC32 (File);
   end Open_File_By_Fd;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (File : in out Elf_File) is
   begin
      Close (File.File);
      File.Fd := Invalid_FD;

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
      Length      : constant Unsigned_16 := File.Ehdr.E_Shnum;
      Size        : File_Size;

   begin
      if Get_Ehdr (File).E_Shentsize /= Elf_Half (Elf_Shdr_Size) then
         raise Error;
      end if;
      if File.Shdr /= null then
         return;
      end if;

      Size := File_Size (Length) * File_Size (Elf_Shdr_Size);
      File.Shdr_Map := Read
        (File    => File.File,
         Offset  => File_Size (File.Ehdr.E_Shoff),
         Length  => Size,
         Mutable => File.Need_Swap);
      File.Shdr := To_Elf_Shdr_Arr_Acc (Data (File.Shdr_Map).all'Address);

      if File_Size (GNATCOLL.Mmap.Last (File.Shdr_Map)) /= Size then
         raise Error;
      end if;

      if File.Need_Swap then
         for Idx in 1 .. File.Ehdr.E_Shnum loop
            Elf_Shdr_Swap (File.Shdr (Idx - 1));
         end loop;
      end if;

      File.Sh_Strtab_Map := Load_Section (File, File.Ehdr.E_Shstrndx);
      File.Sh_Strtab :=
        To_Elf_Strtab_Acc (Data (File.Sh_Strtab_Map).all'Address);
   end Load_Shdr;

   -------------------------------
   -- Enable_Section_Relocation --
   -------------------------------

   procedure Enable_Section_Relocation (File : in out Elf_File) is
   begin
      pragma Assert (File.Shdr /= null);

      Make_Mutable (File, File.Shdr_Map);
      File.Shdr := To_Elf_Shdr_Arr_Acc (Data (File.Shdr_Map).all'Address);
   end Enable_Section_Relocation;

   ------------------
   -- Load_Section --
   ------------------

   function Load_Section
     (File : Elf_File; Index : Elf_Half) return Mapped_Region is
   begin
      return Load_Section (File, Get_Shdr (File, Index));
   end Load_Section;

   ------------------
   -- Load_Section --
   ------------------

   function Load_Section
     (File : Elf_File; Shdr : Elf_Shdr_Acc) return Mapped_Region
   is
      Result : constant Mapped_Region := Read
        (File.File, File_Size (Shdr.Sh_Offset), File_Size (Shdr.Sh_Size));
   begin
      if File_Size (Last (Result)) /= File_Size (Shdr.Sh_Size) then
         raise Error;
      end if;
      return Result;
   end Load_Section;

   ------------------
   -- Make_Mutable --
   ------------------

   procedure Make_Mutable
     (File : Elf_File; Region : in out Mapped_Region) is
   begin
      --  If the region is already mutable (this can happen, for instance, if
      --  it was byte-swapped), do not risk losing changes remapping it.

      if not Is_Mutable (Region) then
         Read
           (File    => File.File,
            Region  => Region,
            Offset  => Offset (Region),
            Length  => File_Size (Last (Region)),
            Mutable => True);
      end if;
   end Make_Mutable;

   --------------
   -- Get_Ehdr --
   --------------

   function Get_Ehdr (File : Elf_File) return Elf_Ehdr is
   begin
      return File.Ehdr.all;
   end Get_Ehdr;

   --------------
   -- Get_Shdr --
   --------------

   function Get_Shdr (File : Elf_File; Index : Elf_Half) return Elf_Shdr_Acc is
   begin
      if Index >= Get_Shdr_Num (File) then
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
      return Get_Shdr (File, Index).Sh_Size;
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
      if I = SHN_UNDEF then
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
      C              : CRC32;
      Content        : Mapped_Region := Read (File.File);
      Content_Length : constant Integer := Integer (Length (File.File));
   begin
      Initialize (C);
      Update (C, String (Data (Content).all (1 .. Content_Length)));
      Free (Content);
      return Get_Value (C);
   end Compute_CRC32;

end Elf_Files;
