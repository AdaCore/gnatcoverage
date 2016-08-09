------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with System.Storage_Elements;

with Dwarf_Handling;
with Outputs;

package body PECoff_Files is

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer;
   --  Read the offset of the PE signature (4 bytes before the COFF header).
   --  Returns 0 in case of error (bad magic number).

   function Extract_Nul_Terminated (S : String) return String;
   --  Return the first characters of S until but not including Nul, or S if
   --  there is no Nul character.

   -----------------------------
   -- Read_Coff_Header_Offset --
   -----------------------------

   function Read_Coff_Header_Offset (Fd : File_Descriptor) return Long_Integer
   is
      MS_Hdr : PEHdr;
   begin
      Lseek (Fd, 0, Seek_Set);

      if Read (Fd, MS_Hdr'Address, PEHdrsz) /= PEHdrsz then
         return 0;
      end if;

      --  Only handle little endian
      if MS_Hdr.E_MZHdr /= MZhdr then
         return 0;
      end if;

      return Long_Integer (MS_Hdr.E_Lfanew);
   end Read_Coff_Header_Offset;

   ----------------
   -- Is_PE_File --
   ----------------

   function Is_PE_File (Fd : File_Descriptor) return Boolean
   is
      Off    : Long_Integer;
      PE_Sig : Unsigned_32;
      Coff_Sig : Unsigned_16;
   begin
      Lseek (Fd, 0, Seek_Set);
      Off := Read_Coff_Header_Offset (Fd);

      if Off = 0 then
         --  A pure COFF file (without the PE header).
         Lseek (Fd, 0, Seek_Set);

         if Read (Fd, Coff_Sig'Address, 2) /= 2 then
            return False;
         end if;

         return Coff_Sig in I386magic | AMD64magic;
      else
         Lseek (Fd, Off, Seek_Set);

         if Read (Fd, PE_Sig'Address, 4) /= 4 then
            return False;
         end if;

         return PE_Sig = Pe_Magic;
      end if;
   end Is_PE_File;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Fd       : File_Descriptor;
      Filename : String_Access)
      return PE_File
   is
      use System.Storage_Elements;

      procedure Exit_With_Error
        (File : in out PE_File; Status : Binary_File_Status; Msg : String);
      --  Assign Status to File, close the file if needed and raise Error with
      --  the filename and Msg.

      ---------------------
      -- Exit_With_Error --
      ---------------------

      procedure Exit_With_Error
        (File : in out PE_File; Status : Binary_File_Status; Msg : String) is
      begin
         Set_Status (File, Status);
         Close_File (File);
         raise Error with File.Filename & ": " & Msg;
      end Exit_With_Error;

      function To_Address is new Ada.Unchecked_Conversion
        (Str_Access, System.Address);

      Hdr_Off     : Long_Integer;
      Opt_Hdr_Off : Long_Integer;
      Opt_Hdr32   : Opt_Hdr_PE32;
      Opt_Hdr64   : Opt_Hdr_PE32_Plus;

   --  Start of processing for Create_File

   begin
      Hdr_Off := Read_Coff_Header_Offset (Fd);

      --  The PE header has three parts: the signature, the file header and the
      --  optional header. Skip the signature, which is assumed to be "PE\0\0".

      if Hdr_Off /= 0 then
         Hdr_Off := Hdr_Off + 4;
      end if;

      return File : PE_File := (Binary_File'(Create_File (Fd, Filename))
                                with others => <>)
      do
         Lseek (Fd, Hdr_Off, Seek_Set);
         if Read (Fd, File.Hdr'Address, Filehdr_Size) /= Filehdr_Size then
            Exit_With_Error
              (File, Status_Read_Error, "failed to read COFF header");
         end if;

         Opt_Hdr_Off := Hdr_Off + Long_Integer (Filehdr_Size);
         if File.Hdr.F_Opthdr >= Unsigned_16 (Opt_Hdr_PE32_Size) then

            --  At this point, we don't know yet if the optional header is a
            --  PE32 one or a PE32+ one. Read a PE32 one and check the magic
            --  number.

            if Read (Fd, Opt_Hdr32'Address, Opt_Hdr_PE32_Size)
                 /= Opt_Hdr_PE32_Size
            then
               Exit_With_Error (File, Status_Read_Error,
                                "failed to read COFF optional header (PE32)");
            end if;

            case Opt_Hdr32.Magic is
               when PE32_Magic =>
                  if File.Hdr.F_Machine /= I386magic then
                     Outputs.Fatal_Error
                       ("Unhandled CPU for PE32:" & File.Hdr.F_Machine'Img);
                  end if;

                  --  We already have the header properly decoded

                  File.Image_Base := Arch.Arch_Addr (Opt_Hdr32.Image_Base);

               when PE32Plus_Magic =>
                  if File.Hdr.F_Machine /= AMD64magic then
                     Outputs.Fatal_Error
                       ("Unhandled CPU for PE32+:" & File.Hdr.F_Machine'Img);
                  end if;

                  --  Re-read the optional header as a PE32+ one

                  Lseek (Fd, Opt_Hdr_Off, Seek_Set);
                  if Read (Fd, Opt_Hdr64'Address, Opt_Hdr_PE32_Plus_Size)
                       /= Opt_Hdr_PE32_Plus_Size
                  then
                     Exit_With_Error
                       (File, Status_Read_Error,
                        "failed to read COFF optional header (PE32+)");
                  end if;
                  File.Image_Base := Arch.Arch_Addr (Opt_Hdr64.Image_Base);

               when others =>
                  Outputs.Fatal_Error
                     ("Invalid optional header magic: " & Opt_Hdr32.Magic'Img);
            end case;
         else
            File.Image_Base := 0;
         end if;

         Set_Nbr_Sections (File, Section_Index (File.Hdr.F_Nscns));

         Read (File.File);
         File.Data := To_Address (Data (File.File));

         --  Map sections.
         File.Scn := To_PE_Scn_Arr_Acc
           (File.Data + Storage_Offset (Hdr_Off)
            + Storage_Offset (File.Hdr.F_Opthdr)
            + Storage_Offset (Filehdr_Size));

         File.Str_Off := File.Hdr.F_Symptr + File.Hdr.F_Nsyms * Symesz;
      end return;
   end Create_File;

   -------------
   -- Get_Hdr --
   -------------

   function Get_Hdr (File : PE_File) return Filehdr is
   begin
      return File.Hdr;
   end Get_Hdr;

   ------------------------
   -- Get_Section_Length --
   ------------------------

   function Get_Section_Length
     (File : PE_File; Index : Section_Index) return Arch.Arch_Addr
   is
      pragma Assert (Index < Section_Index (File.Hdr.F_Nscns));
      Sec : Scnhdr renames File.Scn (Index);
   begin
      --  Contrary to COFF, on PE S_Paddr is the real length
      pragma Assert (Sec.S_Size >= Sec.S_Paddr);
      if Sec.S_Paddr = 0 then
         --  For object files
         return Arch.Arch_Addr (Sec.S_Size);
      else
         --  For images
         return Arch.Arch_Addr (Sec.S_Paddr);
      end if;
   end Get_Section_Length;

   ----------------------------
   -- Extract_Nul_Terminated --
   ----------------------------

   function Extract_Nul_Terminated (S : String) return String is
   begin
      for I in S'Range loop
         if S (I) = ASCII.NUL then
            return S (S'First .. I - 1);
         end if;
      end loop;
      return S;
   end Extract_Nul_Terminated;

   ----------------------
   -- Get_Section_Name --
   ----------------------

   function Get_Section_Name (File : PE_File; Sec : Section_Index)
                             return String
   is
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      Name : String renames File.Scn (Sec).S_Name;
   begin
      if Name (1) = '/' then
         --  Long section name, name in string table
         declare
            Num : Unsigned_32;
         begin
            Num := 0;
            for I in 2 .. Name'Last loop
               exit when Name (I) = ASCII.NUL;
               Num := Num * 10
                 + (Character'Pos (Name (I)) - Character'Pos ('0'));
            end loop;
            return Get_String (File, Num);
         end;
      else
         return Extract_Nul_Terminated (Name);
      end if;
   end Get_Section_Name;

   ----------------
   -- Get_Scnhdr --
   ----------------

   function Get_Scnhdr (File : PE_File; Sec : Section_Index) return Scnhdr is
   begin
      pragma Assert (Sec < Section_Index (File.Hdr.F_Nscns));
      return File.Scn (Sec);
   end Get_Scnhdr;

   ------------------
   -- Load_Section --
   ------------------

   function Load_Section
     (File : PE_File; Index : Section_Index) return Mapped_Region is
      Scn : constant Scnhdr := Get_Scnhdr (File, Index);
      Result : constant Mapped_Region := Read
        (File.File, File_Size (Scn.S_Scnptr), File_Size (Scn.S_Size));
   begin
      if File_Size (Last (Result)) /= File_Size (Scn.S_Size) then
         raise Error;
      end if;
      return Result;
   end Load_Section;

   --------------------
   -- Get_Image_Base --
   --------------------

   function Get_Image_Base (File : PE_File) return Arch.Arch_Addr is
   begin
      return File.Image_Base;
   end Get_Image_Base;

   -----------------
   -- Get_Symbols --
   -----------------

   function Get_Symbols (File : PE_File) return Mapped_Region is
   begin
      return Read (File.File, File_Size (File.Hdr.F_Symptr),
                   File_Size (File.Hdr.F_Nsyms * Symesz));
   end Get_Symbols;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (File : PE_File; Off : Unsigned_32) return String is
      use System.Storage_Elements;
   begin
      return Dwarf_Handling.Read_String
        (File.Data + Storage_Offset (Off + File.Str_Off));
   end Get_String;

   ---------------------
   -- Get_Symbol_Name --
   ---------------------

   function Get_Symbol_Name (File : PE_File; Sym : Syment) return String is
   begin
      if Sym.E.E.E_Zeroes = 0 then
         return Get_String (File, Sym.E.E.E_Offset);
      else
         return Extract_Nul_Terminated (Sym.E.E_Name);
      end if;
   end Get_Symbol_Name;
end PECoff_Files;
