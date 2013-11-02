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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Directories;   use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;

with Coverage.Object;   use Coverage.Object;
with Coverage.Tags;     use Coverage.Tags;
with Disassemblers;     use Disassemblers;
with Dwarf;
with Dwarf_Handling;    use Dwarf_Handling;
with Elf_Disassemblers; use Elf_Disassemblers;
with Execs_Dbase;       use Execs_Dbase;
with Files_Table;       use Files_Table;
with Hex_Images;        use Hex_Images;
with Outputs;
with Perf_Counters;     use Perf_Counters;
with Qemu_Traces;
with Traces_Disa;
with Traces_Lines;      use Traces_Lines;
with Traces_Names;
with Types;             use Types;

package body Traces_Elf is

   function Convert is new Ada.Unchecked_Conversion
     (Str_Access, Binary_Content_Bytes_Acc);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Binary_Content_Bytes_Acc);

   procedure Make_Mutable
     (Exe      : Exe_File_Type;
      Region   : in out Mapped_Region;
      Bin_Cont : out Binary_Content);
   --  Make sure Region (from File) is mutable. Do not remap it if it already
   --  is, so that no existing change is lost.Create a copy of some binary
   --  content and return it.

   No_Stmt_List : constant Unsigned_32 := Unsigned_32'Last;
   --  Value indicating there is no AT_stmt_list

   No_Ranges    : constant Unsigned_32 := Unsigned_32'Last;
   --  Value indicating there is no AT_ranges

   function Get_Strtab_Idx (Exec : Exe_File_Type) return Elf_Half;
   --  Get the section index of the symtab string table.
   --  Return SHN_UNDEF if not found (or in case of error).

   procedure Read_Word8
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_64);
   procedure Read_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_32);
   procedure Read_Word2
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_16);
   procedure Write_Word8
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_64);
   procedure Write_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_32);
   procedure Write_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Integer_32);
   pragma Unreferenced (Write_Word4);
   procedure Read_Address
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Sz   : Natural;
      Res  : out Pc_Type);
   procedure Read_Dwarf_Form_U64
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_64);
   procedure Read_Dwarf_Form_U32
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_32);
   procedure Read_Dwarf_Form_String
     (Exec : in out Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Address);
   procedure Skip_Dwarf_Form
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32);

   procedure Apply_Relocations
     (Exec    : in out Exe_File_Type;
      Sec_Rel : Elf_Half;
      Data    : in out Binary_Content);
   --  Apply relocations from SEC_REL to DATA.
   --  This procedure should only be called to relocate dwarf debug sections,
   --  and therefore handles only a small subset of the relocations. DATA must
   --  be a writable memory area.

   procedure Read_Debug_Lines
     (Exec                  : in out Exe_File_Type;
      Stmt_List_Offset      : Unsigned_32;
      Compilation_Directory : String_Access);
   --  Read the debug lines of a compilation unit.
   --  Stmt_List_Offset is the offset of a stmt list from the beginning of the
   --  .debug_line section of Exec; Compilation_Directory is the value of
   --  DW_AT_comp_dir for the compilation unit, or null if this attribute is
   --  not specified.

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      Region  : out Mapped_Region);
   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      Region  : out Mapped_Region;
      Base    : out Address);
   --  Allocate memory for section SEC of EXEC and read it. LEN is the length
   --  of the section. Loaded bytes will be stored in CONTENT, and the mapped
   --  region it comes from is stored in REGION. It is up to the caller to free
   --  it after use. The low bound of CONTENT is 0.

   procedure Load_Symtab (Exec : in out Exe_File_Type);
   --  Load the symbol table (but not the string table) if not already
   --  loaded.

   Empty_String_Acc : constant String_Access := new String'("");

   function Get_Desc_Set
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return access constant Address_Info_Sets.Set;
   pragma Inline (Get_Desc_Set);
   --  Return the Address_Info_Set of type Kind in Exec containing PC

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Address_Info_Acc) return Boolean is
      pragma Assert (L.Kind = R.Kind);

      function Names_Lt (LN, RN : String_Access) return Boolean;
      --  Compare desginated strings, null is higher than any non-null string

      --------------
      -- Names_Lt --
      --------------

      function Names_Lt (LN, RN : String_Access) return Boolean is
      begin
         if LN = null then
            return False;
         elsif RN = null then
            return True;
         else
            return LN.all < RN.all;
         end if;
      end Names_Lt;

   --  Start of processing for "<"

   begin
      --  Lower start PC sorts lower

      if L.First < R.First then
         return True;
      elsif R.First < L.First then
         return False;
      end if;

      --  Shorter range sorts higher. Note that we use a modular subtraction
      --  instead of a comparison on Last to account for empty ranges with
      --  First = 0 (and Last = all-ones).

      declare
         L_Len : constant Pc_Type := L.Last - L.First + 1;
         R_Len : constant Pc_Type := R.Last - R.First + 1;
      begin
         if R_Len < L_Len then
            return True;
         elsif L_Len < R_Len then
            return False;
         end if;
      end;

      --  Here if L.First = R.First and L.Last = R.Last

      case L.Kind is
         when Section_Addresses =>
            return Names_Lt (L.Section_Name, R.Section_Name);

         when Subprogram_Addresses =>
            return Names_Lt (L.Subprogram_Name, R.Subprogram_Name);

         when Symbol_Addresses =>
            return Names_Lt (L.Symbol_Name, R.Symbol_Name);

         when Line_Addresses =>
            return L.Sloc < R.Sloc;
      end case;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (El : Address_Info_Acc) return String is
      Range_Img : constant String :=
                    Hex_Image (El.First) & '-' & Hex_Image (El.Last);

      function Sloc_Image (Line, Column : Natural) return String;
      --  Return the image of the given sloc. Column info is included only
      --  if Column > 0.

      ----------------
      -- Sloc_Image --
      ----------------

      function Sloc_Image (Line, Column : Natural) return String is
         Line_Img   : constant String := Line'Img;
         Column_Img : constant String := Column'Img;
      begin
         if Column = 0 then
            return Line_Img (Line_Img'First + 1 .. Line_Img'Last);
         else
            return Line_Img (Line_Img'First + 1 .. Line_Img'Last)
              & ':'
              & Column_Img (Column_Img'First + 1 .. Column_Img'Last);
         end if;
      end Sloc_Image;

   --  Start of processing for Image

   begin
      case El.Kind is
         when Section_Addresses =>
            return Range_Img & " section " & El.Section_Name.all;

         when Subprogram_Addresses =>
            return Range_Img & " subprogram " & El.Subprogram_Name.all;

         when Symbol_Addresses =>
            return Range_Img & " symbol for " & El.Symbol_Name.all;

         when Line_Addresses =>
            return Range_Img & " line "
              & Get_Full_Name (El.Sloc.Source_File) & ':'
              & Sloc_Image (Line => El.Sloc.L.Line, Column => El.Sloc.L.Column)
              & (if El.Disc /= 0 then " discriminator" & El.Disc'Img else "");
      end case;
   end Image;

   ------------------
   -- Disp_Address --
   ------------------

   procedure Disp_Address (El : Address_Info_Acc) is
   begin
      Put_Line (Image (El));
   end Disp_Address;

   --------------------
   -- Disp_Addresses --
   --------------------

   procedure Disp_Addresses (Exe : Exe_File_Type; Kind : Address_Info_Kind) is
      use Address_Info_Sets;

      procedure Disp_Address (Cur : Cursor);
      --  Display item at Cur

      ------------------
      -- Disp_Address --
      ------------------

      procedure Disp_Address (Cur : Cursor) is
      begin
         Disp_Address (Element (Cur));
      end Disp_Address;

   --  Start of processing for Disp_Addresses

   begin
      if Kind = Line_Addresses then
         for Subp of Exe.Desc_Sets (Subprogram_Addresses) loop
            Subp.Lines.Iterate (Disp_Address'Access);
         end loop;
      else
         Exe.Desc_Sets (Kind).Iterate (Disp_Address'Access);
      end if;
   end Disp_Addresses;

   ----------------------------
   -- Disp_Compilation_Units --
   ----------------------------

   procedure Disp_Compilation_Units (Exec : Exe_File_Type) is
      use Compile_Unit_Vectors;
      Cu : Compile_Unit_Desc;
      Cur : Cursor;
   begin
      Cur := Exec.Compile_Units.First;

      while Has_Element (Cur) loop
         Cu := Element (Cur);
         Put_Line (Cu.Compile_Unit_Filename.all);
         Next (Cur);
      end loop;
   end Disp_Compilation_Units;

   procedure Insert
     (Set : in out Address_Info_Sets.Set;
      El  : Address_Info_Acc) renames Address_Info_Sets.Insert;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Exec       : out Exe_File_Type;
      Filename   : String;
      Text_Start : Pc_Type)
   is
      Ehdr : Elf_Ehdr;
   begin
      Open_File (Exec.Exe_File, Filename);
      Exec.Exe_Text_Start := Text_Start;
      Ehdr := Get_Ehdr (Exec.Exe_File);
      Exec.Is_Big_Endian := Ehdr.E_Ident (EI_DATA) = ELFDATA2MSB;
      Exec.Exe_Machine := Ehdr.E_Machine;

      if Machine = 0 then
         Machine := Ehdr.E_Machine;

      elsif Machine /= Ehdr.E_Machine then
         --  Mixing different architectures.

         Outputs.Fatal_Error ("unexpected architecture for " & Filename);
      end if;

      --  Be sure the section headers are loaded

      Load_Shdr (Exec.Exe_File);

      for I in 0 .. Get_Shdr_Num (Exec.Exe_File) - 1 loop
         declare
            Name : constant String := Get_Shdr_Name (Exec.Exe_File, I);
         begin
            if Name = ".symtab" then
               Exec.Sec_Symtab := I;

            elsif Name = ".debug_abbrev" then
               Exec.Sec_Debug_Abbrev := I;

            elsif Name = ".debug_info" then
               Exec.Sec_Debug_Info := I;

            elsif Name = ".rela.debug_info" then
               Exec.Sec_Debug_Info_Rel := I;

            elsif Name = ".debug_line" then
               Exec.Sec_Debug_Line := I;

            elsif Name = ".rela.debug_line" then
               Exec.Sec_Debug_Line_Rel := I;

            elsif Name = ".debug_str" then
               Exec.Sec_Debug_Str := I;

            elsif Name = ".debug_ranges" then
               Exec.Sec_Debug_Ranges := I;
            end if;
         end;
      end loop;
   end Open_File;

   --------------------
   -- Close_Exe_File --
   --------------------

   procedure Close_Exe_File (Exec : in out Exe_File_Type) is
   begin
      Close_File (Exec.Exe_File);

      if Exec.Lines_Region /= Invalid_Mapped_Region then
         Free (Exec.Lines_Region);
      end if;
      Exec.Lines_Len := 0;

      if Exec.Symtab_Region /= Invalid_Mapped_Region then
         Free (Exec.Symtab_Region);
      end if;
      Exec.Nbr_Symbols := 0;

      if Exec.Debug_Strs_Region /= Invalid_Mapped_Region then
         Free (Exec.Debug_Strs_Region);
      end if;

      Exec.Debug_Str_Base := Null_Address;
      Exec.Debug_Str_Len := 0;

      Exec.Sec_Symtab         := SHN_UNDEF;
      Exec.Sec_Debug_Abbrev   := SHN_UNDEF;
      Exec.Sec_Debug_Info     := SHN_UNDEF;
      Exec.Sec_Debug_Info_Rel := SHN_UNDEF;
      Exec.Sec_Debug_Line     := SHN_UNDEF;
      Exec.Sec_Debug_Line_Rel := SHN_UNDEF;
      Exec.Sec_Debug_Str      := SHN_UNDEF;
      Exec.Sec_Debug_Ranges   := SHN_UNDEF;
   end Close_Exe_File;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (Exec : in out Exe_File_Type) is
   begin
      Close_Exe_File (Exec);

      --  FIXME: free contents

      for J in Exec.Desc_Sets'Range loop
         Exec.Desc_Sets (J).Clear;
      end loop;
   end Close_File;

   -----------------------
   -- Find_Address_Info --
   -----------------------

   function Find_Address_Info
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Sets.Cursor
   is
      PC_Addr : aliased Address_Info (Kind);
   begin
      --  Empty range with default values sorts higher than any empty range
      --  with non-default values, and higher than any non-empty range,
      --  starting at PC.

      PC_Addr.First := PC;
      PC_Addr.Last  := PC - 1;

      return Set.Floor (PC_Addr'Unchecked_Access);
   end Find_Address_Info;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Exec : Exe_File_Type) return String is
   begin
      return Get_Filename (Exec.Exe_File);
   end Get_Filename;

   -----------------
   -- Get_Machine --
   -----------------

   function Get_Machine (Exec : Exe_File_Type) return Unsigned_16 is
   begin
      return Exec.Exe_Machine;
   end Get_Machine;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Exec : Exe_File_Type) return Long_Integer is
   begin
      return Get_Size (Exec.Exe_File);
   end Get_Size;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (Exec : Exe_File_Type) return GNAT.OS_Lib.OS_Time is
   begin
      return Get_Time_Stamp (Exec.Exe_File);
   end Get_Time_Stamp;

   ---------------
   -- Get_CRC32 --
   ---------------

   function Get_CRC32 (Exec : Exe_File_Type) return Unsigned_32 is
   begin
      return Get_CRC32 (Exec.Exe_File);
   end Get_CRC32;

   ------------------
   -- Get_Desc_Set --
   ------------------

   function Get_Desc_Set
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return access constant Address_Info_Sets.Set
   is
   begin
      if Kind in Exec.Desc_Sets'Range then
         return Exec.Desc_Sets (Kind)'Unchecked_Access;
      else pragma Assert (Kind = Line_Addresses);
         return Get_Address_Info (Exec, Subprogram_Addresses, PC).Lines'Access;
      end if;
   end Get_Desc_Set;

   ----------------------
   -- Time_Stamp_Image --
   ----------------------

   function Time_Stamp_Image (TS : GNAT.OS_Lib.OS_Time) return String is

      use GNAT.OS_Lib;

      function Pad (N, Length : Natural) return String;
      --  Pad the given number with zeros on the left until the given length of
      --  the image is reached.

      ---------
      -- Pad --
      ---------

      function Pad (N, Length : Natural) return String
      is
         Raw_Image   : constant String  := Natural'Image (N);
         First_Idx   : constant Natural :=
            (if Raw_Image (1) = ' ' then 2 else 1);
         Digits_Number : constant Natural := Raw_Image'Length - First_Idx + 1;
         Padding_Len : constant Natural :=
            (if Length > Digits_Number then Length - Digits_Number else 0);
         Padding     : constant String (1 .. Padding_Len) := (others => '0');

      begin
         return Padding & Raw_Image (First_Idx .. Raw_Image'Last);
      end Pad;

   begin
      return
         Pad (Integer (GM_Year (TS)), 0)
         & "-" & Pad (Natural (GM_Month (TS)), 2)
         & "-" & Pad (Natural (GM_Day (TS)), 2)
         & " " & Pad (Natural (GM_Hour (TS)), 2)
         & ":" & Pad (Natural (GM_Minute (TS)), 2)
         & ":" & Pad (Natural (GM_Second (TS)), 2);
   end Time_Stamp_Image;

   ----------------------------
   -- Match_Trace_Executable --
   ----------------------------

   function Match_Trace_Executable
     (Exec : Exe_File_Type; Trace_File : Trace_File_Type)
     return String
   is
      use Qemu_Traces;

      Trace_Exe_Size  : constant String := Get_Info
                                             (Trace_File, Exec_File_Size);
      Trace_Exe_TS    : constant String := Get_Info
                                             (Trace_File,
                                              Exec_File_Time_Stamp);
      Trace_Exe_CRC32 : constant String := Get_Info
                                             (Trace_File,
                                              Exec_File_CRC32);

      File_Size  : constant String := Long_Integer'Image (Get_Size (Exec));
      File_TS    : constant String := Time_Stamp_Image (Get_Time_Stamp (Exec));
      File_CRC32 : constant String := Unsigned_32'Image (Get_CRC32 (Exec));
   begin
      if Trace_Exe_Size /= "" and then Trace_Exe_Size /= File_Size then
         return
            "ELF file is" & File_Size
            & " bytes long, but trace indicates" & Trace_Exe_Size;

      elsif Trace_Exe_TS /= "" and then Trace_Exe_TS /= File_TS then
         return
            "ELF file created on " & File_TS
            & " but trace indicates " & Trace_Exe_TS;

      elsif Trace_Exe_CRC32 /= "" and then Trace_Exe_CRC32 /= File_CRC32 then
         return
            "ELF file CRC32 checksum is " & File_CRC32
            & " but trace indicates " & Trace_Exe_CRC32;

      else
         return "";
      end if;
   end Match_Trace_Executable;

   --------------------
   -- Get_Strtab_Idx --
   --------------------

   function Get_Strtab_Idx (Exec : Exe_File_Type) return Elf_Half is
      Symtab_Shdr : Elf_Shdr_Acc;
   begin
      if Exec.Sec_Symtab = SHN_UNDEF then
         return SHN_UNDEF;
      end if;

      Symtab_Shdr := Get_Shdr (Exec.Exe_File, Exec.Sec_Symtab);

      if Symtab_Shdr.Sh_Type /= SHT_SYMTAB
        or else Symtab_Shdr.Sh_Link = 0
        or else Natural (Symtab_Shdr.Sh_Entsize) /= Elf_Sym_Size
      then
         return SHN_UNDEF;
      else
         return Elf_Half (Symtab_Shdr.Sh_Link);
      end if;
   end Get_Strtab_Idx;

   ----------------
   -- Read_Word8 --
   ----------------

   procedure Read_Word8
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_64)
   is
   begin
      if Exec.Is_Big_Endian then
         Read_Word8_Be (Base, Off, Res);
      else
         Read_Word8_Le (Base, Off, Res);
      end if;
   end Read_Word8;

   ----------------
   -- Read_Word4 --
   ----------------

   procedure Read_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_32)
   is
   begin
      if Exec.Is_Big_Endian then
         Read_Word4_Be (Base, Off, Res);
      else
         Read_Word4_Le (Base, Off, Res);
      end if;
   end Read_Word4;

   ----------------
   -- Read_Word2 --
   ----------------

   procedure Read_Word2
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_16)
   is
   begin
      if Exec.Is_Big_Endian then
         Read_Word2_Be (Base, Off, Res);
      else
         Read_Word2_Le (Base, Off, Res);
      end if;
   end Read_Word2;

   -----------------
   -- Write_Word8 --
   -----------------

   procedure Write_Word8
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_64)
   is
   begin
      if Exec.Is_Big_Endian then
         Write_Word8_Be (Base, Off, Val);
      else
         Write_Word8_Le (Base, Off, Val);
      end if;
   end Write_Word8;

   -----------------
   -- Write_Word4 --
   -----------------

   procedure Write_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_32)
   is
   begin
      if Exec.Is_Big_Endian then
         Write_Word4_Be (Base, Off, Val);
      else
         Write_Word4_Le (Base, Off, Val);
      end if;
   end Write_Word4;

   -----------------
   -- Write_Word4 --
   -----------------

   procedure Write_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Integer_32)
   is
      function To_Unsigned_32 is
        new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   begin
      Write_Word4 (Exec, Base, Off, To_Unsigned_32 (Val));
   end Write_Word4;

   ------------------
   -- Read_Address --
   ------------------

   procedure Read_Address
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Sz   : Natural;
      Res  : out Pc_Type)
   is
   begin
      if Sz /= Natural (Pc_Type_Size) then
         raise Program_Error with "address size mismatch";
      end if;

      if Sz = 4 then
         declare
            V : Unsigned_32;
         begin
            Read_Word4 (Exec, Base, Off, V);
            Res := Pc_Type (V);
         end;

      elsif Sz = 8 then
         declare
            V : Unsigned_64;
         begin
            Read_Word8 (Exec, Base, Off, V);
            Res := Pc_Type (V);
         end;

      else
         raise Program_Error with "unhandled address length";
      end if;
   end Read_Address;

   -------------------------
   -- Read_Dwarf_Form_U64 --
   -------------------------

   procedure Read_Dwarf_Form_U64
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_64)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_addr =>
            declare
               V : Pc_Type;
            begin
               Read_Address (Exec, Base, Off, Exec.Addr_Size, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_flag =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_data1 =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_data2 =>
            declare
               V : Unsigned_16;
            begin
               Read_Word2 (Exec, Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_data4
            | DW_FORM_ref4 =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Exec, Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_data8 =>
            Read_Word8 (Exec, Base, Off, Res);

         when DW_FORM_sdata =>
            declare
               V : Unsigned_32;
            begin
               Read_SLEB128 (Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_udata =>
            declare
               V : Unsigned_32;
            begin
               Read_ULEB128 (Base, Off, V);
               Res := Unsigned_64 (V);
            end;

         when DW_FORM_strp
           | DW_FORM_string
           | DW_FORM_block1 =>
            raise Program_Error;

         when others =>
            raise Program_Error;
      end case;
   end Read_Dwarf_Form_U64;

   -------------------------
   -- Read_Dwarf_Form_U32 --
   -------------------------

   procedure Read_Dwarf_Form_U32
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_32)
   is
      R : Unsigned_64;
   begin
      Read_Dwarf_Form_U64 (Exec, Base, Off, Form, R);
      Res := Unsigned_32 (R);
   end Read_Dwarf_Form_U32;

   ----------------------------
   -- Read_Dwarf_Form_String --
   ----------------------------

   procedure Read_Dwarf_Form_String
     (Exec : in out Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Address)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_strp =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Exec, Base, Off, V);
               if Exec.Debug_Str_Base = Null_Address then
                  Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Str,
                                          Exec.Debug_Str_Len,
                                          Exec.Debug_Strs,
                                          Exec.Debug_Strs_Region);
                  if Exec.Sec_Debug_Str = SHN_UNDEF then
                     return;
                  end if;
                  Exec.Debug_Str_Base := Address_Of (Exec.Debug_Strs, 0);
               end if;
               Res := Exec.Debug_Str_Base + Storage_Offset (V);
            end;

         when DW_FORM_string =>
            Res := Base + Off;
            declare
               C : Unsigned_8;
            begin
               loop
                  Read_Byte (Base, Off, C);
                  exit when C = 0;
               end loop;
            end;

         when others =>
            Put ("???");
            raise Program_Error;
      end case;
   end Read_Dwarf_Form_String;

   ---------------------
   -- Skip_Dwarf_Form --
   ---------------------

   procedure Skip_Dwarf_Form
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_addr =>
            Off := Off + Storage_Offset (Exec.Addr_Size);

         when DW_FORM_block1 =>
            declare
               V : Unsigned_8;
            begin
               Read_Byte (Base, Off, V);
               Off := Off + Storage_Offset (V);
            end;

         when DW_FORM_block2 =>
            declare
               V : Unsigned_16;
            begin
               Read_Word2 (Exec, Base, Off, V);
               Off := Off + Storage_Offset (V);
            end;

         when DW_FORM_block4 =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Exec, Base, Off, V);
               Off := Off + Storage_Offset (V);
            end;

         when DW_FORM_flag
           | DW_FORM_data1 =>
            Off := Off + 1;

         when DW_FORM_data2 =>
            Off := Off + 2;

         when DW_FORM_data4
            | DW_FORM_ref4
            | DW_FORM_strp =>
            Off := Off + 4;

         when DW_FORM_data8 =>
            Off := Off + 8;

         when DW_FORM_sdata =>
            declare
               V : Unsigned_32;
            begin
               Read_SLEB128 (Base, Off, V);
            end;

         when DW_FORM_udata =>
            declare
               V : Unsigned_32;
            begin
               Read_ULEB128 (Base, Off, V);
            end;

         when DW_FORM_string =>
            declare
               C : Unsigned_8;
            begin
               loop
                  Read_Byte (Base, Off, C);
                  exit when C = 0;
               end loop;
            end;

         when others =>
            Put_Line ("Unhandled dwarf form #" & Unsigned_32'Image (Form));
            raise Program_Error;
      end case;
   end Skip_Dwarf_Form;

   -----------------------
   -- Apply_Relocations --
   -----------------------

   procedure Apply_Relocations
     (Exec    : in out Exe_File_Type;
      Sec_Rel : Elf_Half;
      Data    : in out Binary_Content)
   is
      Relocs_Len    : Elf_Addr;
      Relocs        : Binary_Content;
      Relocs_Region : Mapped_Region;
      Relocs_Base   : Address;

      Sym_Num       : Unsigned_32;
      Sym           : Elf_Sym;

      Shdr          : Elf_Shdr_Acc;
      Off           : Storage_Offset;

      Offset        : Elf_Addr;
      R             : Elf_Rela;

   begin
      --  The only sections on which we have to apply relocations are the
      --  .debug_info and the .debug_line sections. These sections seem to have
      --  relocations in object code files only (before linking). In these,
      --  sections do not have their address assigned yet, and thus we can
      --  consider that they are located at 0x0.

      --  We noticed that in these cases (relocations for debug sections in
      --  object code files), the symbols targetted by the relocations are
      --  sections themselves, and they do not have any addend (.rel.*
      --  relocation sections).

      --  So in this particular configuration, there is no need to relocate
      --  debug sections since it would only add section addresses (= 0) to
      --  the offsets already present in the debug sections. Thus, we do not
      --  handle relocation sections without addend.

      Shdr := Get_Shdr (Exec.Exe_File, Sec_Rel);
      if Shdr.Sh_Type /= SHT_RELA then
         raise Program_Error;
      end if;
      if Natural (Shdr.Sh_Entsize) /= Elf_Rela_Size then
         raise Program_Error;
      end if;
      if Shdr.Sh_Size mod Pc_Type (Elf_Rela_Size) /= 0 then
         raise Program_Error;
      end if;
      Alloc_And_Load_Section
        (Exec, Sec_Rel, Relocs_Len, Relocs, Relocs_Region, Relocs_Base);
      if Relocs_Len /= Shdr.Sh_Size then
         raise Program_Error;
      end if;

      Load_Symtab (Exec);

      Off := 0;
      while Off < Storage_Offset (Relocs_Len) loop
         --  Read relocation entry

         R := Get_Rela
           (Exec.Exe_File,
            Address_Of (Relocs, Elf_Addr (Off)));
         Off := Off + Storage_Offset (Elf_Rela_Size);

         if R.R_Offset > Data.Last then
            raise Program_Error with "relocation offset beyond section size";
         end if;

         Sym_Num := Elf_R_Sym (R.R_Info);
         if Sym_Num > Unsigned_32 (Exec.Nbr_Symbols) then
            raise Program_Error with "invalid symbol number in relocation";
         end if;
         Sym := Get_Sym
           (Exec.Exe_File,
            Address_Of
              (Exec.Symtab, Elf_Addr (Sym_Num) * Elf_Addr (Elf_Sym_Size)));

         if Elf_St_Type (Sym.St_Info) = STT_SECTION then
            Offset := Get_Shdr (Exec.Exe_File,
                                Sym.St_Shndx).Sh_Addr;
         else
            --  Also relocate global/local symbols ???
            Offset := 0;
         end if;

         case Exec.Exe_Machine is
            when EM_X86_64 =>
               case Elf_R_Type (R.R_Info) is
                  when R_X86_64_NONE =>
                     null;
                  when R_X86_64_64 =>
                     --  When compiled in 64-bit mode, Elf_Addr is a subtype of
                     --  Unsigned_64, so the following conversion is redundant.
                     --  However, is is needed when compiling in 32-bit mode,
                     --  in which Elf_Addr is a subtype of Unsigned_32.

                     pragma Warnings (Off);
                     Write_Word8 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_64 (Offset
                                               + Elf_Addr (R.R_Addend)));
                     pragma Warnings (On);
                  when R_X86_64_32 =>
                     --  There is no need to disable the warnings for the
                     --  following conversion to Unsigned_32 even in 32-bit
                     --  mode since it is considered by the compiler as a
                     --  "disambiguation mean" between the unsigned/signed
                     --  Write_Word4 functions.

                     Write_Word4 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_32 (Offset
                                               + Elf_Addr (R.R_Addend)));
                  when others =>
                     raise Program_Error with
                        ("unhandled x86_64 relocation, reloc is "
                         & Elf_Word'Image (Elf_R_Type (R.R_Info)));
               end case;
            when EM_PPC =>
               case Elf_R_Type (R.R_Info) is
                  when R_PPC_ADDR32 =>
                     Write_Word4 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_32 (Offset
                                               + Elf_Addr (R.R_Addend)));
                  when R_PPC_NONE =>
                     null;
                  when others =>
                     raise Program_Error with "unhandled PPC relocation";
               end case;
            when EM_SPARC =>
               case Elf_R_Type (R.R_Info) is
                  when R_SPARC_UA32 =>
                     Write_Word4 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_32 (Offset
                                               + Elf_Addr (R.R_Addend)));
                  when others =>
                     raise Program_Error with "unhandled SPARC relocation";
               end case;
            when others =>
               Outputs.Fatal_Error
                 ("Relocs unhandled for this machine, reloc is"
                  & Elf_Word'Image (Elf_R_Type (R.R_Info)));
               raise Program_Error;
         end case;

      end loop;
      Free (Relocs_Region);
   end Apply_Relocations;

   ----------------------------
   -- Alloc_And_Load_Section --
   ----------------------------

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      Region  : out Mapped_Region)
   is
      Sec_Len : Elf_Addr;
   begin
      if Sec /= SHN_UNDEF then
         Sec_Len := Get_Section_Length (Exec.Exe_File, Sec);
         pragma Assert (Sec_Len > 0);

         Len := Sec_Len;
         Region := Load_Section (Exec.Exe_File, Sec);
         Content.Content := Convert (Data (Region));
         if Sec_Len > 0 then
            Content.First := 0;
            Content.Last := Sec_Len - 1;
         else
            Content.First := 1;
            Content.Last := 0;
         end if;
      end if;
   end Alloc_And_Load_Section;

   ----------------------------
   -- Alloc_And_Load_Section --
   ----------------------------

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      Region  : out Mapped_Region;
      Base    : out Address)
   is
   begin
      if Sec /= SHN_UNDEF then
         Alloc_And_Load_Section (Exec, Sec, Len, Content, Region);
         Base := Address_Of (Content, 0);
      else
         Content := (null, 0, 0);
         Base := Null_Address;
         Len := 0;
      end if;
   end Alloc_And_Load_Section;

   --  Extract lang, subprogram name and stmt_list (offset in .debug_line).
   --  What does this comment apply to???

   -------------------------------
   -- Build_Debug_Compile_Units --
   -------------------------------

   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type) is
      use Dwarf;
      use Compile_Unit_Vectors;

      Abbrev_Len     : Elf_Addr;
      Abbrevs        : Binary_Content;
      Abbrevs_Region : Mapped_Region;
      Abbrev_Base    : Address;
      Map            : Abbrev_Map_Acc;
      Abbrev         : Address;

      Info_Len              : Elf_Addr;
      Infos                 : Binary_Content;
      Infos_Region          : Mapped_Region;
      Base                  : Address;
      Off, Sec_Off, Tag_Off : Storage_Offset;
      Aoff                  : Storage_Offset;

      Len : Unsigned_32;
      Ver : Unsigned_16;
      Abbrev_Off : Unsigned_32;
      Ptr_Sz : Unsigned_8;
      Last : Storage_Offset;
      Num : Unsigned_32;

      Tag : Unsigned_32;
      Name : Unsigned_32;
      Form : Unsigned_32;

      Level : Unsigned_8;

      At_Sib             : Unsigned_64 := 0;
      At_Stmt_List       : Unsigned_32 := No_Stmt_List;
      At_Ranges          : Unsigned_32 := No_Ranges;
      At_Low_Pc          : Unsigned_64 := 0;
      At_High_Pc         : Unsigned_64 := 0;
      At_Lang            : Unsigned_64 := 0;
      At_Name            : Address := Null_Address;
      At_Comp_Dir        : Address := Null_Address;
      At_Linkage_Name    : Address := Null_Address;
      At_Abstract_Origin : Unsigned_64 := 0;
      Cu_Base_Pc         : Unsigned_64;

      Current_Sec     : Address_Info_Acc;
      Current_Subprg  : Address_Info_Acc;
      Current_CU      : CU_Id := No_CU_Id;
      Current_DIE_CU  : DIE_CU_Id := No_DIE_CU_Id;
      Compilation_Dir : String_Access;
      Unit_Filename   : String_Access;
      Subprg_Low      : Pc_Type;

      --  The generation of the mapping: call site -> target function (for
      --  indirect calls) is done in two steps: first accumulate information as
      --  tags and attributes comes from the debug information, then bind data
      --  into Exec.

      package Subprg_DIE_To_PC_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Storage_Offset,
         Element_Type => Pc_Type);

      type Call_Target is record
         To_PC             : Pc_Type;
         Target_Subprg_Tag : Storage_Offset;
      end record;

      package Call_Site_To_Target_Maps is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Call_Target);

      Subprg_To_PC : Subprg_DIE_To_PC_Maps.Map;
      Call_Site_To_Target : Call_Site_To_Target_Maps.Vector;

   begin
      --  Return now if already loaded

      if not Exec.Compile_Units.Is_Empty then
         return;
      end if;

      if Exec.Desc_Sets (Section_Addresses).Is_Empty then
         --  The file may have no code

         return;
      end if;

      --  Load .debug_abbrev

      Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Abbrev,
                              Abbrev_Len, Abbrevs,
                              Abbrevs_Region, Abbrev_Base);

      Map := null;

      --  Load .debug_info

      Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Info,
                              Info_Len, Infos,
                              Infos_Region, Base);

      --  Load symbols

      Build_Symbols (Exec'Unchecked_Access);

      if Exec.Sec_Debug_Info_Rel /= SHN_UNDEF then
         Make_Mutable (Exec, Infos_Region, Infos);
         Base := Address_Of (Infos, 0);
         Apply_Relocations (Exec, Exec.Sec_Debug_Info_Rel, Infos);
      end if;

      Off := 0;
      while Off < Storage_Offset (Info_Len) loop
         --  Read .debug_info header:
         --    Length, version, offset in .debug_abbrev, pointer size.

         Sec_Off := Off;
         Read_Word4 (Exec, Base, Off, Len);
         Last := Off + Storage_Offset (Len);
         Read_Word2 (Exec, Base, Off, Ver);
         Read_Word4 (Exec, Base, Off, Abbrev_Off);
         Read_Byte (Base, Off, Ptr_Sz);
         exit when Ver not in 2 .. 3;
         Level := 0;

         Exec.Addr_Size := Natural (Ptr_Sz);
         Cu_Base_Pc := 0;

         Build_Abbrev_Map (Abbrev_Base + Storage_Offset (Abbrev_Off), Map);

         --  Read DIEs

         loop
         <<Again>>
            exit when Off >= Last;
            Tag_Off := Off;
            Read_ULEB128 (Base, Off, Num);
            if Num = 0 then
               Level := Level - 1;
               goto Again;
            end if;
            if Num <= Map.all'Last then
               Abbrev := Map (Num);
            else
               Abbrev := Null_Address;
            end if;
            if Abbrev = Null_Address then
               Put ("!! abbrev #" & Hex_Image (Num) & " does not exist !!");
               New_Line;
               return;
            end if;

            --  Read tag

            Aoff := 0;
            Read_ULEB128 (Abbrev, Aoff, Tag);

            if Read_Byte (Abbrev + Aoff) /= 0 then
               Level := Level + 1;
            end if;

            --  Skip child

            Aoff := Aoff + 1;

            --  Read attributes

            loop
               Read_ULEB128 (Abbrev, Aoff, Name);
               Read_ULEB128 (Abbrev, Aoff, Form);
               exit when Name = 0 and Form = 0;

               case Name is
                  when DW_AT_sibling =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_Sib);
                  when DW_AT_name =>
                     Read_Dwarf_Form_String (Exec, Base, Off, Form, At_Name);
                  when DW_AT_comp_dir =>
                     Read_Dwarf_Form_String (Exec, Base, Off, Form,
                                             At_Comp_Dir);
                  when DW_AT_MIPS_linkage_name | DW_AT_linkage_name =>
                     Read_Dwarf_Form_String (Exec, Base, Off, Form,
                                             At_Linkage_Name);
                  when DW_AT_stmt_list =>
                     Read_Dwarf_Form_U32 (Exec, Base, Off, Form, At_Stmt_List);
                  when DW_AT_ranges =>
                     Read_Dwarf_Form_U32 (Exec, Base, Off, Form, At_Ranges);
                  when DW_AT_low_pc =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_Low_Pc);
                     if Form /= DW_FORM_addr then
                        At_Low_Pc := At_Low_Pc + Cu_Base_Pc;
                     end if;
                  when DW_AT_high_pc =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_High_Pc);
                     if Form /= DW_FORM_addr then
                        At_High_Pc := At_High_Pc + Cu_Base_Pc;
                     end if;
                  when DW_AT_language =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_Lang);
                  when DW_AT_abstract_origin =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form,
                                          At_Abstract_Origin);

                     --  References to other DIEs are relative to the beginning
                     --  of the current compile unit.

                     At_Abstract_Origin :=
                        Unsigned_64 (Sec_Off) + At_Abstract_Origin;
                  when others =>
                     Skip_Dwarf_Form (Exec, Base, Off, Form);
               end case;
            end loop;

            case Tag is
               when DW_TAG_compile_unit =>
                  if At_Comp_Dir /= Null_Address then
                     Compilation_Dir := new String'(Read_String (At_Comp_Dir));
                  else
                     Compilation_Dir := null;
                  end if;

                  Unit_Filename := new String'(Read_String (At_Name));
                  Current_CU    := Comp_Unit (Base_Name (Unit_Filename.all));
                  Exec.Compile_Units.Append
                    (Compile_Unit_Desc'(Unit_Filename,
                                        Compilation_Dir,
                                        At_Stmt_List,
                                        Pc_Type (At_Low_Pc),
                                        Pc_Type (At_High_Pc)));
                  Current_DIE_CU := DIE_CU_Id (Exec.Compile_Units.Length);

                  if At_Ranges /= No_Ranges then
                     Cu_Base_Pc := 0;

                  elsif At_Low_Pc = 0 and At_High_Pc = 0 then
                     --  This field are not required
                     Cu_Base_Pc := 0;

                  else
                     Cu_Base_Pc := At_Low_Pc;
                  end if;

                  At_Lang := 0;
                  At_Stmt_List := No_Stmt_List;

               when DW_TAG_subprogram =>
                  if At_High_Pc > At_Low_Pc then
                     --  This subprogram is present in this compile unit

                     Subprg_Low := Exec.Exe_Text_Start + Pc_Type (At_Low_Pc);
                     if Current_Sec = null
                       or else
                       Subprg_Low not in Current_Sec.First .. Current_Sec.Last
                     then
                        Current_Sec := Get_Address_Info
                          (Exec, Section_Addresses, Subprg_Low);
                     end if;

                     if Current_Sec = null then
                        --  Can this happen???

                        raise Program_Error with "no section for subprogram";
                     end if;

                     Current_Subprg :=
                       new Address_Info'
                       (Kind              => Subprogram_Addresses,
                        First             => Subprg_Low,
                        Last              =>
                          Exec.Exe_Text_Start + Pc_Type (At_High_Pc - 1),
                        Parent            => Current_Sec,
                        Subprogram_Name   =>
                          new String'(Read_String (At_Name)),
                        Subprogram_CU     => Current_CU,
                        Subprogram_DIE_CU => Current_DIE_CU,
                        Lines             => Address_Info_Sets.Empty_Set);
                     Exec.Desc_Sets (Subprogram_Addresses).
                       Insert (Current_Subprg);
                     Subprg_To_PC.Insert (Tag_Off, Pc_Type (At_Low_Pc));

                  elsif At_Linkage_Name /= Null_Address
                           or else
                        At_Name /= Null_Address
                  then
                     --  Missing subprograms can be referenced by call sites:
                     --  collect their addresses.

                     if At_Linkage_Name = Null_Address then
                        At_Linkage_Name := At_Name;
                     end if;

                     --  We assume that the symbol referenced by the name
                     --  attribute is present in the symbol table as a
                     --  STB_GLOBAL symbol.

                     declare
                        use Symbol_To_PC_Maps;
                        Subprg_Sym : constant Symbol := To_Symbol
                          (Read_String (At_Linkage_Name));
                        Cur : constant Symbol_To_PC_Maps.Cursor :=
                           Exec.Symbol_To_PC.Find (Subprg_Sym);
                     begin
                        --  Sometimes, subprogram DIEs references a symbol that
                        --  is not present. In these case, just ignore them.

                        if Cur /= Symbol_To_PC_Maps.No_Element then
                           Subprg_To_PC.Insert
                             (Tag_Off, Symbol_To_PC_Maps.Element (Cur));
                        end if;
                     end;
                  end if;

               when DW_TAG_GNU_call_site =>
                  if At_Low_Pc /= 0 and then At_Abstract_Origin /= 0 then
                     Call_Site_To_Target.Append
                       ((Pc_Type (At_Low_Pc),
                        Storage_Offset (At_Abstract_Origin)));
                  end if;

               when others =>
                  null;
            end case;
            At_Low_Pc := 0;
            At_High_Pc := 0;
            At_Ranges := No_Ranges;
            At_Abstract_Origin := 0;

            At_Name := Null_Address;
            At_Comp_Dir := Null_Address;
            At_Linkage_Name := Null_Address;
         end loop;
         Free (Map);
      end loop;

      --  If there is no debug information in this binary, the following
      --  sections may not have been loaded.

      if Infos_Region /= Invalid_Mapped_Region then
         Free (Infos_Region);
      end if;
      if Abbrevs_Region /= Invalid_Mapped_Region then
         Free (Abbrevs_Region);
      end if;

      --  Fill the map: call site -> target function, using accumulated
      --  information.

      for Call_To_Target of Call_Site_To_Target loop
         declare
            use Subprg_DIE_To_PC_Maps;
            Cur : constant Subprg_DIE_To_PC_Maps.Cursor :=
               Subprg_To_PC.Find (Call_To_Target.Target_Subprg_Tag);
         begin
            if Cur /= Subprg_DIE_To_PC_Maps.No_Element then
               Exec.Call_Site_To_Target.Insert
                 (Call_To_Target.To_PC, Subprg_DIE_To_PC_Maps.Element (Cur));
            end if;
         end;
      end loop;
   end Build_Debug_Compile_Units;

   -----------------
   -- Load_Symtab --
   -----------------

   procedure Load_Symtab (Exec : in out Exe_File_Type) is
      Symtab_Shdr : Elf_Shdr_Acc;
      Symtab_Len : Elf_Addr;
   begin
      if Exec.Nbr_Symbols /= 0 then
         --  Already loaded.
         return;
      end if;

      if Exec.Sec_Symtab = SHN_UNDEF then
         raise Program_Error with "no symbol table";
      end if;

      Alloc_And_Load_Section (Exec, Exec.Sec_Symtab,
                              Symtab_Len, Exec.Symtab, Exec.Symtab_Region);
      Symtab_Shdr := Get_Shdr (Exec.Exe_File, Exec.Sec_Symtab);
      if Symtab_Shdr.Sh_Type /= SHT_SYMTAB
        or else Symtab_Shdr.Sh_Link = 0
        or else Natural (Symtab_Shdr.Sh_Entsize) /= Elf_Sym_Size
      then
         raise Program_Error with "invalid symbol table section";
      end if;
      if Symtab_Shdr.Sh_Size /= Symtab_Len
        or else Symtab_Shdr.Sh_Size mod Elf_Addr (Elf_Sym_Size) /= 0
      then
         raise Program_Error with "invalid symtab size";
      end if;
      Exec.Nbr_Symbols := Natural (Symtab_Len) / Elf_Sym_Size;
   end Load_Symtab;

   package Filenames_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => String_Access,
      "=" => "=");

   package File_Indices_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Source_File_Index);

   ----------------------
   -- Read_Debug_Lines --
   ----------------------

   procedure Read_Debug_Lines
     (Exec                  : in out Exe_File_Type;
      Stmt_List_Offset      : Unsigned_32;
      Compilation_Directory : String_Access)
   is
      use Dwarf;
      Base : Address;
      Off  : Storage_Offset;

      type Opc_Length_Type is array (Unsigned_8 range <>) of Unsigned_8;
      type Opc_Length_Acc is access Opc_Length_Type;
      Opc_Length : Opc_Length_Acc;

      procedure Free is
        new Ada.Unchecked_Deallocation (Opc_Length_Type, Opc_Length_Acc);

      Total_Len    : Unsigned_32;
      Version      : Unsigned_16;
      Prolog_Len   : Unsigned_32;
      Min_Insn_Len : Unsigned_8;
      Dflt_Is_Stmt : Unsigned_8;
      Line_Base    : Unsigned_8;
      Line_Range   : Unsigned_8;
      Opc_Base     : Unsigned_8;

      B   : Unsigned_8;
      Arg : Unsigned_32;

      Old_Off   : Storage_Offset;
      File_Dir  : Unsigned_32;
      File_Time : Unsigned_32;
      File_Len  : Unsigned_32;

      Ext_Len : Unsigned_32;
      Ext_Opc : Unsigned_8;

      Last : Storage_Offset;

      --  Base_Pc is there to memorize the PC at which a sequence starts, per
      --  DW_LNE_set_address statements. This is null at the start of ranges
      --  discarded by gc-section and we need to discard the relative entries
      --  that follow as well.

      Pc, Base_Pc  : Pc_Type;
      File         : Natural;
      Line, Column : Unsigned_32;
      Line_Base2   : Unsigned_32;
      Disc         : Unsigned_32;

      Nbr_Dirnames  : Unsigned_32;
      Nbr_Filenames : Unsigned_32;
      Dirnames      : Filenames_Vectors.Vector;
      Filenames     : Filenames_Vectors.Vector;

      File_Indices  : File_Indices_Vectors.Vector;
      --  Cached file indices for Filenames.

      Cur_Subprg,
      Cur_Sec    : Address_Info_Sets.Cursor;
      Subprg,
      Sec        : Address_Info_Acc;
      --  Current subprogram and section

      procedure Set_Parents (PC : Pc_Type);
      --  Set the current subprogram and section for PC

      Last_Line     : Address_Info_Acc := null;

      procedure Reset_Lines;
      procedure New_Source_Line;
      procedure Close_Source_Line;
      --  Need comments???

      -----------------------
      -- Close_Source_Line --
      -----------------------

      procedure Close_Source_Line is
      begin
         if Last_Line = null then
            return;
         end if;

         --  Set the last PC for this line

         Last_Line.Last := Exec.Exe_Text_Start + Pc - 1;

         --  Work-around a gc-section issue: there may be an empty line
         --  statement at address 0 (because it was discarded). Avoid setting
         --  Last to 0xffff_ffff as it would cover all the executable.
         --  FIXME: discard the whole block ?

         if Last_Line.Last = Pc_Type'Last and then Last_Line.First = 0 then
            Last_Line.First := 1;
            Last_Line.Last  := 0;
         end if;

         --  If this entry has a non-empty range, mark it as such using the
         --  Is_Last flag, and propagate the range to all entries with the same
         --  start address and an empty range.

         if Last_Line.Last >= Last_Line.First then
            Last_Line.Is_Last := True;

            for Info of Get_Address_Infos
              (Subprg.Lines, Line_Addresses, Last_Line.First)
            loop
               if Info.Last < Info.First then
                  pragma Assert (not Info.Is_Last);
                  Info.Last := Last_Line.Last;
               end if;
            end loop;
         end if;

         Last_Line := null;
      end Close_Source_Line;

      ---------------------
      -- New_Source_Line --
      ---------------------

      procedure New_Source_Line is
         use Address_Info_Sets;

         Pos        : Cursor;
         Inserted   : Boolean;
         File_Index : Source_File_Index;
      begin

         --  Discard 0-relative entries in exec files, corresponding to
         --  regions garbage collected by gc-section.

         if Base_Pc = 0 and then Get_Ehdr (Exec.Exe_File).E_Type = ET_EXEC then
            return;
         end if;

         Close_Source_Line;

         --  Note: Last will be updated by Close_Source_Line

         File_Index := File_Indices.Element (File);
         if File_Index = No_Source_File then
            --  Compute the file index for this source file if it's not cached
            --  yet.

            File_Index := Get_Index_From_Full_Name
               (Filenames.Element (File).all);
            File_Indices.Replace_Element (File, File_Index);
         end if;

         Set_Parents (Exec.Exe_Text_Start + Pc);
         if Subprg /= null then
            Last_Line :=
              new Address_Info'
                (Kind    => Line_Addresses,
                 First   => Exec.Exe_Text_Start + Pc,
                 Last    => Exec.Exe_Text_Start + Pc,
                 Parent  => (if Subprg /= null then Subprg else Sec),
                 Sloc    =>
                   (Source_File  => File_Index,
                    L            => (Line   => Natural (Line),
                                     Column => Natural (Column))),
                 Disc    => Disc,
                 Is_Last => False);

            Subprg.Lines.Insert (Last_Line, Pos, Inserted);
            if not Inserted then

               --  An empty line has already been inserted at PC. Merge it with
               --  current line.

               Last_Line := Element (Pos);
            end if;
         end if;
         Disc := 0;
      end New_Source_Line;

      -----------------
      -- Reset_Lines --
      -----------------

      procedure Reset_Lines is
      begin
         Base_Pc := 0;
         Pc      := 0;
         File    := 1;
         Line    := 1;
         Column  := 0;
         Disc    := 0;
      end Reset_Lines;

      -----------------
      -- Set_Parents --
      -----------------

      procedure Set_Parents (PC : Pc_Type) is
         use Address_Info_Sets;

         procedure Set_Parent
           (Kind  : Address_Info_Kind;
            Cur   : in out Cursor;
            Cache : in out Address_Info_Acc);

         ----------------
         -- Set_Parent --
         ----------------

         procedure Set_Parent
           (Kind  : Address_Info_Kind;
            Cur   : in out Cursor;
            Cache : in out Address_Info_Acc)
         is
         begin
            if Cache = null or else PC < Cache.First then
               Cur := Find_Address_Info (Exec.Desc_Sets (Kind), Kind, PC);
            end if;

            if Cur = No_Element then
               Cache := null;
            else
               while Cur /= No_Element loop
                  Cache := Element (Cur);
                  exit when PC in Cache.First .. Cache.Last;
                  Next (Cur);
               end loop;
            end if;
         end Set_Parent;

      begin
         Set_Parent (Section_Addresses,    Cur_Sec,    Sec);
         Set_Parent (Subprogram_Addresses, Cur_Subprg, Subprg);
      end Set_Parents;

   --  Start of processing for Read_Debug_Lines

   begin
      --  Load .debug_line

      if not Is_Loaded (Exec.Lines) then
         Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Line,
                                 Exec.Lines_Len, Exec.Lines,
                                 Exec.Lines_Region, Base);
         if Exec.Sec_Debug_Line_Rel /= SHN_UNDEF then
            Make_Mutable (Exec, Exec.Lines_Region, Exec.Lines);
            Apply_Relocations (Exec, Exec.Sec_Debug_Line_Rel, Exec.Lines);
         end if;
      end if;

      Off := Storage_Offset (Stmt_List_Offset);
      if Off >= Storage_Offset (Exec.Lines_Len) then
         return;
      end if;

      Base := Address_Of (Exec.Lines, 0);

      --  Read header

      Read_Word4 (Exec, Base, Off, Total_Len);
      Last := Off + Storage_Offset (Total_Len);
      Read_Word2 (Exec, Base, Off, Version);
      Read_Word4 (Exec, Base, Off, Prolog_Len);
      Read_Byte (Base, Off, Min_Insn_Len);
      Read_Byte (Base, Off, Dflt_Is_Stmt);
      Read_Byte (Base, Off, Line_Base);
      Read_Byte (Base, Off, Line_Range);
      Read_Byte (Base, Off, Opc_Base);

      --  Initial state registers

      Reset_Lines;

      Line_Base2 := Unsigned_32 (Line_Base);
      if (Line_Base and 16#80#) /= 0 then
         Line_Base2 := Line_Base2 or 16#Ff_Ff_Ff_00#;
      end if;
      Opc_Length := new Opc_Length_Type (1 .. Opc_Base - 1);
      for I in 1 .. Opc_Base - 1 loop
         Read_Byte (Base, Off, Opc_Length (I));
      end loop;

      --  Include directories

      Nbr_Dirnames := 0;
      Filenames_Vectors.Clear (Dirnames);
      loop
         B := Read_Byte (Base + Off);
         exit when B = 0;
         Filenames_Vectors.Append
           (Dirnames, new String'(Read_String (Base + Off)));
         Read_String (Base, Off);
         Nbr_Dirnames := Nbr_Dirnames + 1;
      end loop;
      Off := Off + 1;

      --  File names

      Nbr_Filenames := 0;
      Filenames_Vectors.Clear (Filenames);
      File_Indices.Clear;
      loop
         B := Read_Byte (Base + Off);
         exit when B = 0;
         Old_Off := Off;
         Read_String (Base, Off);
         Read_ULEB128 (Base, Off, File_Dir);

         declare
            Filename : constant String := Read_String (Base + Old_Off);
            Dir      : String_Access;
         begin
            if File_Dir /= 0
              and then File_Dir <= Nbr_Dirnames
            then
               Dir := Filenames_Vectors.Element (Dirnames, Integer (File_Dir));

            elsif Compilation_Directory /= null
              and then not GNAT.OS_Lib.Is_Absolute_Path (Filename)
            then
               Dir := Compilation_Directory;

            else
               Dir := Empty_String_Acc;
            end if;

            Filenames_Vectors.Append
              (Filenames, Build_Filename (Dir.all, Filename));

            --  Do not get file index for this filename until necessary (see
            --  the New_Source_Line procedure). This prevents file table
            --  cluttering with unused filenames.

            File_Indices.Append (No_Source_File);
         end;

         Read_ULEB128 (Base, Off, File_Time);
         Read_ULEB128 (Base, Off, File_Len);
         Nbr_Filenames := Nbr_Filenames + 1;
      end loop;
      Off := Off + 1;

      while Off < Last loop

         --  Read code

         Read_Byte (Base, Off, B);
         Old_Off := Off;

         if B = 0 then

            --  Extended opcode

            Read_ULEB128 (Base, Off, Ext_Len);
            Old_Off := Off;
            Read_Byte (Base, Off, Ext_Opc);
            case Ext_Opc is
               when DW_LNE_end_sequence =>
                  Close_Source_Line;
                  Reset_Lines;

               when DW_LNE_set_address =>
                  Read_Address
                    (Exec, Base, Off, Elf_Arch.Elf_Addr'Size / 8, Pc);
                  Base_Pc := Pc;

               when DW_LNE_define_file =>
                  raise Program_Error with "DW_LNE_define_file unhandled";

               when DW_LNE_set_discriminator =>
                  Read_ULEB128 (Base, Off, Disc);

               when others =>
                  raise Program_Error
                    with "unhandled DW_LNE" & Unsigned_8'Image (Ext_Opc);
            end case;
            Off := Old_Off + Storage_Offset (Ext_Len);

         elsif B < Opc_Base then

            --  Standard opcode

            case B is
               when DW_LNS_copy =>
                  New_Source_Line;

               when DW_LNS_advance_pc =>
                  Read_ULEB128 (Base, Off, Arg);
                  Pc := Pc + Pc_Type (Arg * Unsigned_32 (Min_Insn_Len));

               when DW_LNS_advance_line =>
                  Read_SLEB128 (Base, Off, Arg);
                  Line := Line + Arg;

               when DW_LNS_set_file =>
                  Read_ULEB128 (Base, Off, Arg);
                  File := Natural (Arg);

               when DW_LNS_set_column =>
                  Read_ULEB128 (Base, Off, Column);

               when DW_LNS_negate_stmt     |
                    DW_LNS_set_basic_block =>
                  null;

               when DW_LNS_const_add_pc =>
                  Pc := Pc + Pc_Type
                    (Unsigned_32 ((255 - Opc_Base) / Line_Range)
                       * Unsigned_32 (Min_Insn_Len));

               when DW_LNS_fixed_advance_pc =>
                  raise Program_Error with "DW_LNS_fixed_advance_pc unhandled";

               when DW_LNS_set_prologue_end   |
                    DW_LNS_set_epilogue_begin |
                    DW_LNS_set_isa            =>
                  null;

               when others =>

                  --  Instruction length

                  for J in 1 .. Opc_Length (B) loop
                     Read_ULEB128 (Base, Off, Arg);
                  end loop;
            end case;

         else

            --  Special opcode

            B := B - Opc_Base;
            Pc := Pc + Pc_Type (Unsigned_32 (B / Line_Range)
                                  * Unsigned_32 (Min_Insn_Len));
            Line := Line + Line_Base2 + Unsigned_32 (B mod Line_Range);
            New_Source_Line;
         end if;
      end loop;

      if Last_Line /= null then
         raise Program_Error with "missing end_of_sequence";
      end if;

      Free (Opc_Length);
   end Read_Debug_Lines;

   -----------------------
   -- Build_Debug_Lines --
   -----------------------

   procedure Build_Debug_Lines (Exec : in out Exe_File_Type) is
   begin
      --  Return now if already loaded

      if Exec.Lines_Region /= Invalid_Mapped_Region then
         return;
      end if;

      --  Be sure compile units are loaded

      Build_Debug_Compile_Units (Exec);

      --  Read all .debug_line

      for Cu of Exec.Compile_Units loop
         Read_Debug_Lines (Exec, Cu.Stmt_List, Cu.Compilation_Directory);
      end loop;
   end Build_Debug_Lines;

   ---------------------
   --  Build_Sections --
   ---------------------

   procedure Build_Sections (Exec : in out Exe_File_Type) is
      Shdr : Elf_Shdr_Acc;
      Addr : Pc_Type;
      Last : Pc_Type;
      Offset : Pc_Type;
      Do_Reloc : Boolean;
   begin
      --  Return now if already built

      if not Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      --  Iterate over all section headers

      Offset := 0;
      Do_Reloc := Get_Ehdr (Exec.Exe_File).E_Type = ET_REL;

      if Do_Reloc then
         Enable_Section_Relocation (Exec.Exe_File);
      end if;

      for Idx in 0 .. Get_Shdr_Num (Exec.Exe_File) - 1 loop
         Shdr := Get_Shdr (Exec.Exe_File, Idx);

         --  Only A+X sections are interesting.

         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
           and then Shdr.Sh_Size > 0
         then
            Addr := Pc_Type (Shdr.Sh_Addr + Offset);
            Last := Pc_Type (Shdr.Sh_Addr + Offset + Shdr.Sh_Size - 1);

            if Do_Reloc then
               --  Relocate the sections, so that they won't overlap.
               --  This is when the executable is a partially linked
               --  with section per function binary (such as VxWorks DKM).
               --
               --  Note that section are not slided by Exe_Text_Start ???
               Shdr.Sh_Addr := Shdr.Sh_Addr + Offset;
               Offset := (Last + Shdr.Sh_Addralign - 1)
                 and not (Shdr.Sh_Addralign - 1);
            end if;

            Insert (Exec.Desc_Sets (Section_Addresses),
                    new Address_Info'
                    (Kind            => Section_Addresses,
                     First           => Addr,
                     Last            => Last,
                     Parent          => null,
                     Section_Name    => new String'(
                                          Get_Shdr_Name (Exec.Exe_File, Idx)),
                     Section_Index   => Idx,
                     Section_Content => Invalid_Binary_Content,
                     Section_Region  => Invalid_Mapped_Region));
         end if;
      end loop;
   end Build_Sections;

   --------------
   -- Get_Sloc --
   --------------

   function Get_Sloc
     (Set : Address_Info_Sets.Set;
      PC   : Pc_Type) return Source_Location
   is
      SL : constant Source_Locations :=
             Get_Slocs (Set, PC, Last_Only => True);
   begin
      if SL'Length = 0 then
         return Slocs.No_Location;

      else
         pragma Assert (SL'Length = 1);
         return SL (1);
      end if;
   end Get_Sloc;

   ---------------
   -- Get_Slocs --
   ---------------

   function Get_Slocs
     (Set       : Address_Info_Sets.Set;
      PC        : Pc_Type;
      Last_Only : Boolean := False) return Source_Locations
   is
      use Address_Info_Sets;

      Line_Infos : constant Address_Info_Arr :=
                           Get_Address_Infos (Set, Line_Addresses, PC);
      Result     : Source_Locations (1 .. Natural (Line_Infos'Length));
      Last       : Natural := Result'First - 1;

   begin
      for Addr_Info of Line_Infos loop
         if Addr_Info.Last >= Addr_Info.First
              and then
            (Addr_Info.Is_Last or else not Last_Only)
         then
            Last := Last + 1;
            Result (Last) := Addr_Info.Sloc;
         end if;
      end loop;

      return Result (Result'First .. Last);
   end Get_Slocs;

   ---------------------
   -- Get_Call_Target --
   ---------------------

   function Get_Call_Target
     (Exec     : Exe_File_Type;
      PC       : Pc_Type;
      Call_Len : Pc_Type) return Pc_Type
   is
      use Call_Site_To_Target_Maps;
      Cur : constant Cursor := Exec.Call_Site_To_Target.Find (PC + Call_Len);
   begin
      if Cur = No_Element then
         return No_PC;
      else
         return Element (Cur);
      end if;
   end Get_Call_Target;

   ----------------------
   -- Get_Compile_Unit --
   ----------------------

   procedure Get_Compile_Unit
     (Exec : Exe_File_Type;
      PC   : Pc_Type;
      CU_Filename, CU_Directory : out String_Access)
   is
      use Compile_Unit_Vectors;
      Subp_Info : constant Address_Info_Acc :=
         Get_Address_Info (Exec, Subprogram_Addresses, PC);
      CU        : Compile_Unit_Desc;
   begin
      if Subp_Info = null
         or else Subp_Info.all.Subprogram_DIE_CU = No_DIE_CU_Id
      then
         CU_Filename := null;
         CU_Directory := null;
      else
         CU := Exec.Compile_Units.Element (Subp_Info.all.Subprogram_DIE_CU);
         CU_Filename := CU.Compile_Unit_Filename;
         CU_Directory := CU.Compilation_Directory;
      end if;
   end Get_Compile_Unit;

   --------------------------
   -- Load_Section_Content --
   --------------------------

   procedure Load_Section_Content
     (Exec : Exe_File_Type;
      Sec  : Address_Info_Acc)
   is
      Len : Elf_Addr;
   begin
      if not Is_Loaded (Sec.Section_Content) then
         Alloc_And_Load_Section
           (Exec, Sec.Section_Index,
            Len, Sec.Section_Content, Sec.Section_Region);
         Relocate (Sec.Section_Content, Sec.First);
      end if;
   end Load_Section_Content;

   --------------------------
   -- Load_Code_And_Traces --
   --------------------------

   procedure Load_Code_And_Traces
     (Exec : Exe_File_Acc;
      Base : access Traces_Base)
   is
      use Address_Info_Sets;

      Cur : Cursor;
      Sym : Address_Info_Acc;
      Sec : Address_Info_Acc;

      Subp_Key : Traces_Names.Subprogram_Key;
   begin
      if Is_Empty (Exec.Desc_Sets (Symbol_Addresses)) then
         return;
      end if;

      --  Iterate on symbols

      Cur := Exec.Desc_Sets (Symbol_Addresses).First;
      while Cur /= No_Element loop
         Sym := Element (Cur);

         --  If the symbol is not to be covered, skip it

         if Traces_Names.Is_Routine_Of_Interest (Sym.Symbol_Name.all) then

            --  Be sure the section is loaded

            Sec := Sym.Parent;
            Load_Section_Content (Exec.all, Sec);

            --  Add the code and trace information to the symbol's entry in the
            --  routines database.

            Traces_Names.Key_From_Symbol (Exec, Sym, Subp_Key);
            Traces_Names.Add_Routine (Subp_Key, Exec, Sym.Symbol_Tag);

            begin
               Traces_Names.Add_Code_And_Traces
                 (Subp_Key,
                  Exec,
                  Slice (Sec.Section_Content, Sym.First, Sym.Last),
                  Base);
            exception
               when others =>
                  Disp_Address (Sym);
                  raise;
            end;
         end if;

         Next (Cur);
      end loop;
   end Load_Code_And_Traces;

   ------------------------------------
   -- Build_Source_Lines_For_Section --
   ------------------------------------

   procedure Build_Source_Lines_For_Section
     (Exec    : Exe_File_Acc;
      Base    : Traces_Base_Acc;
      Section : Binary_Content)
   is
      use Address_Info_Sets;

      Cur         : Cursor;
      Subprg      : Address_Info_Acc;
      Source_File : Source_File_Index := No_Source_File;

      Init_Line_State : Line_State;

      PC_Addr : aliased Address_Info (Subprogram_Addresses);

   begin
      --  Find subprogram at PC
      --  Note: the following assumes that Section'First is non-zero

      PC_Addr.First := Section.First - 1;
      PC_Addr.Last  := Section.First - 1;

      --  Find line info with lowest start address that is strictly greater
      --  than Section'First - 1.

      Cur := Find_Address_Info
        (Exec.Desc_Sets (Subprogram_Addresses),
         Subprogram_Addresses,
         Section.First);

      --  Iterate on lines

      while Cur /= No_Element loop
         Subprg := Element (Cur);

         --  Only consider subprograms that are in Section (i.e. whose First
         --  address is in Section'Range).

         exit when Subprg.First > Section.Last;

         for Line of Element (Cur).Lines loop
            if Line.First >= Section.First then

               --  Get corresponding file (check previous file for speed-up)

               if Line.Sloc.Source_File /= Source_File then
                  Source_File := Line.Sloc.Source_File;
               end if;

               if Base = null then
                  Init_Line_State := No_Code;
               else
                  Init_Line_State :=
                    Get_Line_State (Base.all, Line.First, Line.Last);
               end if;

               if Object_Coverage_Enabled then
                  Add_Line_For_Object_Coverage
                    (Source_File,
                     Init_Line_State,
                     Line.Sloc.L.Line,
                     Line,
                     Base,
                     Exec);
               end if;
            end if;
         end loop;
         Next (Cur);
      end loop;
   end Build_Source_Lines_For_Section;

   --------------------
   -- Set_Insn_State --
   --------------------

   procedure Set_Insn_State
     (Base : in out Traces_Base; Section : Binary_Content)
   is
      use Address_Info_Sets;

      function Coverage_State (State : Insn_State) return Insn_State;
      --  Given the branch coverage state of an instruction, return the state
      --  that corresponds to the actual coverage action xcov is performing.

      --------------------
      -- Coverage_State --
      --------------------

      function Coverage_State (State : Insn_State) return Insn_State is
      begin
         if Enabled (Insn) then
            --  Instruction coverage; no need to trace which ways a branch
            --  has been covered.

            if State = Branch_Taken
              or else State = Both_Taken
              or else State = Fallthrough_Taken
            then
               return Covered;
            else
               return State;
            end if;

         else
            --  Branch coverage; nothing to do.
            --  In any other case (source coverage), the actual state will be
            --  computed later, based on the branch coverage results and
            --  the source coverage obligations.
            --  Later = where???
            return State;
         end if;
      end Coverage_State;

      It    : Entry_Iterator;
      Trace : Trace_Entry;

      Last_Pc_1, Last_Pc_2             : Pc_Type;
      Last_Insn_1_Len, Last_Insn_2_Len : Pc_Type;
      --  Addresses and length of the (respectively) penultimate and last
      --  instructions of the current trace entry.

      Is_Cond        : Boolean;
      First_Cond_Pc  : Pc_Type;
      First_Cond_Len : Pc_Type;
      Cond_State     : Insn_State;

      Next_Pc        : Pc_Type;
      --  Temporary instruction address, used only when looking for last
      --  instructions.

      Branch               : Branch_Kind;
      Flag_Indir           : Boolean;
      Branch_Dest, FT_Dest : Dest;
      --  Unused, but mandatory arguments when getting instructions properties

   --  Start of processing for Set_Insn_State

   begin
      Init_Post (Base, It, Section.First);
      Get_Next_Trace (Trace, It);

      while Trace /= Bad_Trace loop

         --  First, search the two last instructions (two to handle the delay
         --  slot if needed).

         Last_Pc_1 := No_PC;
         Last_Pc_2 := Trace.First;
         loop
            Last_Insn_2_Len := Pc_Type
              (Disa_For_Machine (Machine).Get_Insn_Length
                (Slice (Section, Last_Pc_2, Trace.Last)));
            Next_Pc := Last_Pc_2 + Last_Insn_2_Len;
            exit when Next_Pc = Trace.Last + 1;

            --  Crash if something got wrong... We should arrive right after
            --  the end of the trace entry instructions range.

            if Next_Pc > Trace.Last then
               raise Program_Error;
            end if;
            Last_Pc_1 := Last_Pc_2;
            Last_Insn_1_Len := Last_Insn_2_Len;
            Last_Pc_2 := Next_Pc;
         end loop;

         --  Then take (into First_Cond_Pc and First_Cond_Len) the first
         --  conditionally executed instruction of these two.

         --  Note: if there is no delay slot (like in x86* or in PowerPC), the
         --  penultimate instruction will not be a conditionnal one anyway:
         --  traces are basic blocks or splitted ones, so there is at most one
         --  conditionnal expression per trace, and if there is one, it is at
         --  the end of the trace.

         First_Cond_Pc := No_PC;
         if Last_Pc_1 /= No_PC then
            Disa_For_Machine (Machine).Get_Insn_Properties
              (Slice (Section, Last_Pc_1, Last_Pc_1 + Last_Insn_1_Len - 1),
               Last_Pc_1,
               Branch,
               Flag_Indir,
               Is_Cond,
               Branch_Dest, FT_Dest);
            if Is_Cond then
               First_Cond_Pc := Last_Pc_1;
               First_Cond_Len := Last_Insn_1_Len;
            end if;
         end if;
         if First_Cond_Pc = No_PC then
            Disa_For_Machine (Machine).Get_Insn_Properties
              (Slice (Section, Last_Pc_2, Last_Pc_2 + Last_Insn_2_Len - 1),
               Last_Pc_2,
               Branch,
               Flag_Indir,
               Is_Cond,
               Branch_Dest, FT_Dest);
            First_Cond_Pc := Last_Pc_2;
            First_Cond_Len := Last_Insn_2_Len;
         end if;

         --  Tag the code before the first branch as covered, and split the
         --  condition away if needed: unconditionnal and conditionnal
         --  instructions can have a different coverage states.

         if Is_Cond and then First_Cond_Pc > Trace.First then
            Split_Trace
              (Base, It, First_Cond_Pc - 1, Coverage_State (Covered));
         else
            Update_State (Base, It, Coverage_State (Covered));
         end if;

         --  If there is a conditionnal instruction at all, compute its state

         if Is_Cond then
            case Trace.Op and 2#111# is
               when 0 => Cond_State := Covered;
               when 1 => Cond_State := Branch_Taken;
               when 2 => Cond_State := Fallthrough_Taken;
               when 3 => Cond_State := Both_Taken;
               when others =>
                  raise Program_Error with
                    ("Invalid flags combination: "
                     & Unsigned_8'Image (Trace.Op));
            end case;
            Update_State (Base, It, Coverage_State (Cond_State));

            --  And if the conditionnal was the penultimate instruction of the
            --  original trace, split it again to tag the last instruction as
            --  covered: when there is a delay slot, there cannot be two
            --  consecutive branch instructions.

            if First_Cond_Pc = Last_Pc_1 then
               Split_Trace
                 (Base, It,
                  First_Cond_Pc + First_Cond_Len - 1,
                  Coverage_State (Cond_State));
               Update_State (Base, It, Coverage_State (Covered));
            end if;
         end if;

         Get_Next_Trace (Trace, It);
      end loop;
   end Set_Insn_State;

   -------------------
   -- Build_Symbols --
   -------------------

   procedure Build_Symbols (Exec : Exe_File_Acc) is
      use Address_Info_Sets;

      type Addr_Info_Acc_Arr is array (0 .. Get_Shdr_Num (Exec.Exe_File))
        of Address_Info_Acc;
      Sections_Info : Addr_Info_Acc_Arr := (others => null);
      Sec : Address_Info_Acc;

      Symtab_Base : Address;
      Do_Reloc : Boolean;

      Strtab_Idx     : Elf_Half;
      Strtab_Len     : Elf_Addr;
      Strtabs        : Binary_Content;
      Strtabs_Region : Mapped_Region;
      ESym           : Elf_Sym;
      Offset         : Pc_Type;

      Sym_Type : Unsigned_8;
      Sym      : Address_Info_Acc;

      Cur : Cursor;
      Ok : Boolean;

   --  Start of processing for Build_Symbols

   begin
      --  Build_Sections must be called before

      if Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      if not Exec.Desc_Sets (Symbol_Addresses).Is_Empty then
         return;
      end if;

      --  Fill the Sections_Info array

      Cur := First (Exec.Desc_Sets (Section_Addresses));
      while Has_Element (Cur) loop
         Sec := Element (Cur);
         Sections_Info (Sec.Section_Index) := Sec;
         Next (Cur);
      end loop;

      --  Load symtab and strtab

      if Exec.Sec_Symtab = SHN_UNDEF then
         return;
      end if;
      Load_Symtab (Exec.all);
      Symtab_Base := Address_Of (Exec.Symtab, 0);

      Strtab_Idx := Get_Strtab_Idx (Exec.all);
      if Strtab_Idx = SHN_UNDEF then
         return;
      end if;
      Alloc_And_Load_Section (Exec.all, Strtab_Idx,
                              Strtab_Len, Strtabs, Strtabs_Region);

      Do_Reloc := Get_Ehdr (Exec.Exe_File).E_Type = ET_REL;
      Offset := Exec.Exe_Text_Start;

      for I in 1 .. Exec.Nbr_Symbols loop
         ESym := Get_Sym
           (Exec.Exe_File,
            Symtab_Base + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (ESym.St_Info);

         if  (Sym_Type = STT_FUNC or else Sym_Type = STT_NOTYPE)
           and then ESym.St_Shndx in Sections_Info'Range
           and then Sections_Info (ESym.St_Shndx) /= null
           and then ESym.St_Size > 0
         then
            if Do_Reloc then
               --  Relocate symbols
               Offset := Exec.Exe_Text_Start
                 + Sections_Info (ESym.St_Shndx).First;
            end if;

            Sym := new Address_Info'
              (Kind          => Symbol_Addresses,
               First         => Offset + Pc_Type (ESym.St_Value),
               Last          => Offset + Pc_Type
                                           (ESym.St_Value
                                            + Elf_Addr (ESym.St_Size) - 1),
               Parent        => Sections_Info (ESym.St_Shndx),
               Symbol_Name   => new String'
                 (Read_String (Address_Of (Strtabs, Elf_Addr (ESym.St_Name)))),
               others        => <>);

            Address_Info_Sets.Insert
              (Exec.Desc_Sets (Symbol_Addresses), Sym, Cur, Ok);
         end if;

         --  We might need to resolve function symbols even when they belong to
         --  no section (external) in the current compile unit.

         if (Sym_Type = STT_FUNC or else Sym_Type = STT_NOTYPE)
              and then
            Elf_St_Bind (ESym.St_Info) = STB_GLOBAL
         then
            declare
               use Ada.Strings.Fixed;

               Name     : constant String := Read_String
                 (Address_Of (Strtabs, Elf_Addr (ESym.St_Name)));
               At_Index : Natural := Index (Name, "@@", Name'First);

            begin
               --  If there is a "@@" pattern and if it does not start the
               --  symbol name, strip it and the right part of the symbol name.

               if At_Index = 1 or else At_Index = 0 then
                  At_Index := Name'Last + 1;
               end if;
               Exec.Symbol_To_PC.Insert
                 (To_Symbol (Name (1 .. At_Index - 1)),
                  Offset + Pc_Type (ESym.St_Value));
            end;
         end if;
      end loop;

      Free (Strtabs_Region);
   end Build_Symbols;

   ----------------------
   -- Get_Address_Info --
   ----------------------

   function Get_Address_Info
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Acc
   is
      Addr_Infos : constant Address_Info_Arr :=
                     Get_Address_Infos (Set, Kind, PC);
   begin
      if Addr_Infos'Length = 0 then
         return null;
      else
         return Addr_Infos (Addr_Infos'First);
      end if;
   end Get_Address_Info;

   function Get_Address_Info
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Acc
   is
     (Get_Address_Info (Get_Desc_Set (Exec, Kind, PC).all, Kind, PC));

   -----------------------
   -- Get_Address_Infos --
   -----------------------

   type AI_Cache_Entry is record
      Last      : Address_Info_Sets.Cursor;
      Last_Set  : access constant Address_Info_Sets.Set;
      Last_Info : Address_Info_Acc;
   end record;

   AI_Cache : array (Address_Info_Kind) of AI_Cache_Entry;

   function Get_Address_Infos
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Arr
   is
      use Address_Info_Sets;

      Cache : AI_Cache_Entry renames AI_Cache (Kind);

      Prev, Last : Cursor;
      Count      : Natural := 0;

   begin
      --  First check whether results for the last lookup still match

      if Cache.Last /= No_Element
           and then
         Set'Unchecked_Access = Cache.Last_Set
           and then
         (PC in Cache.Last_Info.First .. Cache.Last_Info.Last
          or else PC = Cache.Last_Info.First)
      then
         --  Cache hit

         Bump (Addr_Map_Cache_Hit);

         pragma Assert (Cache.Last = Find_Address_Info (Set, Kind, PC));

      else
         --  Cache miss

         Bump (Addr_Map_Cache_Miss);

         Cache.Last := No_Element;
      end if;

      if Cache.Last = No_Element then
         --  Cache entry is stale, perform lookup again (note: the call to
         --  Floor is costly).

         Cache.Last := Find_Address_Info (Set, Kind, PC);

         if Cache.Last /= No_Element then
            Cache.Last_Info := Element (Cache.Last);
            Cache.Last_Set  := Set'Unchecked_Access;
         end if;
      end if;

      Last := Cache.Last;
      Prev := Last;

      loop
         exit when Prev = No_Element;

         declare
            Prev_Info : constant Address_Info_Acc := Element (Prev);
         begin
            exit when not (Prev_Info.First <= PC
                           and then (Prev_Info.First > Prev_Info.Last
                                     or else Prev_Info.Last >= PC));
         end;

         Count := Count + 1;
         Previous (Prev);
      end loop;

      return Result : Address_Info_Arr (1 .. Count) do
         while Count > 0 loop
            Result (Count) := Element (Last);
            Previous (Last);
            Count := Count - 1;
         end loop;
      end return;
   end Get_Address_Infos;

   function Get_Address_Infos
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Arr is
     (Get_Address_Infos (Get_Desc_Set (Exec, Kind, PC).all, Kind, PC));

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Exec : Exe_File_Type;
      PC   : Pc_Type) return Address_Info_Acc
   is
   begin
      return Get_Address_Info (Exec, Symbol_Addresses, PC);
   end Get_Symbol;

   ---------------
   -- Symbolize --
   ---------------

   procedure Symbolize
     (Sym      : Exe_File_Type;
      Pc       : Traces.Pc_Type;
      Line     : in out String;
      Line_Pos : in out Natural)
   is
      procedure Add (C : Character);
      --  Add C to the line

      procedure Add (Str : String);
      --  Add STR to the line

      Symbol : constant Address_Info_Acc := Get_Symbol (Sym, Pc);

      ---------
      -- Add --
      ---------

      procedure Add (C : Character) is
      begin
         if Line_Pos <= Line'Last then
            Line (Line_Pos) := C;
            Line_Pos := Line_Pos + 1;
         end if;
      end Add;

      ---------
      -- Add --
      ---------

      procedure Add (Str : String) is
      begin
         for I in Str'Range loop
            Add (Str (I));
         end loop;
      end Add;

   --  Start of processing for Symbolize

   begin
      if Symbol = null then
         return;
      end if;

      Add (" <");
      Add (Symbol.Symbol_Name.all);
      if Pc /= Symbol.First then
         Add ('+');
         Add (Hex_Image (Pc - Symbol.First));
      end if;
      Add ('>');
   end Symbolize;

   -------------------
   -- Init_Iterator --
   -------------------

   procedure Init_Iterator
     (Exe  : Exe_File_Type;
      Kind : Address_Info_Kind;
      It   : out Addresses_Iterator)
   is
      use Address_Info_Sets;
   begin
      It.Cur := Exe.Desc_Sets (Kind).First;
   end Init_Iterator;

   -------------------
   -- Next_Iterator --
   -------------------

   procedure Next_Iterator
     (It : in out Addresses_Iterator; Addr : out Address_Info_Acc)
   is
      use Address_Info_Sets;
   begin
      if It.Cur = No_Element then
         Addr := null;
      else
         Addr := Element (It.Cur);
         Next (It.Cur);
      end if;
   end Next_Iterator;

   ------------------------
   -- Build_Source_Lines --
   ------------------------

   procedure Build_Source_Lines is
      use Traces_Names;

      procedure Build_Source_Lines_For_Routine
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info);
      --  Build source line information from debug information for the given
      --  routine.

      ------------------------------------
      -- Build_Source_Lines_For_Routine --
      ------------------------------------

      procedure Build_Source_Lines_For_Routine
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Key);
      begin
         if Info.Exec /= null and then Is_Loaded (Info.Insns) then
            Tag_Provider.Enter_Routine (Info);
            Build_Debug_Lines (Info.Exec.all);
            Build_Source_Lines_For_Section
              (Info.Exec, Info.Traces, Info.Insns);
         end if;
      end Build_Source_Lines_For_Routine;

   --  Start of processing for Build_Source_Lines

   begin
      Iterate (Build_Source_Lines_For_Routine'Access);
   end Build_Source_Lines;

   -------------------------------
   -- Build_Routines_Insn_State --
   -------------------------------

   procedure Build_Routines_Insn_State is
      use Traces_Names;

      procedure Build_Routine_Insn_State
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info);
      --  Set trace state for the given routine

      ------------------------------
      -- Build_Routine_Insn_State --
      ------------------------------

      procedure Build_Routine_Insn_State
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Key);
      begin
         if Is_Loaded (Info.Insns) then
            Set_Insn_State (Info.Traces.all, Info.Insns);
         end if;
      end Build_Routine_Insn_State;

   --  Start of processing for Build_Routines_Insn_State

   begin
      Iterate (Build_Routine_Insn_State'Access);
   end Build_Routines_Insn_State;

   --------------------------
   -- Disassemble_File_Raw --
   --------------------------

   procedure Disassemble_File_Raw (File : in out Exe_File_Type) is
      use Address_Info_Sets;

      procedure Local_Disassembler (Cur : Cursor);
      --  Comment needed???

      ------------------------
      -- Local_Disassembler --
      ------------------------

      procedure Local_Disassembler (Cur : Cursor) is
         Pc       : Pc_Type;
         Insn_Len : Natural := 0;
         Sec      : constant Address_Info_Acc := Element (Cur);
         Insns    : Binary_Content;
         Line_Pos : Natural;
         Line     : String (1 .. 128);
      begin
         Load_Section_Content (File, Sec);
         Put_Line ("section " & Sec.Section_Name.all);
         Insns := Sec.Section_Content;
         Pc := Insns.First;

         while Pc <= Insns.Last loop
            Put (Hex_Image (Pc));
            Put (":");
            Put (ASCII.HT);

            Disa_For_Machine (Machine).
              Disassemble_Insn (Slice (Insns, Pc, Insns.Last), Pc,
                                Line, Line_Pos, Insn_Len, File);

            for I in Pc .. Pc + Pc_Type (Insn_Len - 1) loop
               Put (Hex_Image (Get (Insns, I)));
               Put (' ');
            end loop;

            for I in Insn_Len .. 3 loop
               Put ("   ");
            end loop;

            Put ("  ");
            Put (Line (Line'First .. Line_Pos - 1));
            New_Line;

            Pc := Pc + Pc_Type (Insn_Len);
            exit when Pc = 0;
         end loop;
      end Local_Disassembler;

   --  Start of processing for Disassemble_File_Raw

   begin
      Build_Sections (File);
      Build_Symbols (File'Unchecked_Access);
      File.Desc_Sets (Section_Addresses).Iterate (Local_Disassembler'Access);
   end Disassemble_File_Raw;

   ----------------------
   -- Disassemble_File --
   ----------------------

   procedure Disassemble_File (File : in out Exe_File_Type) is
      use Address_Info_Sets;
      Cur : Cursor;
      Sec : Address_Info_Acc;
      Addr : Pc_Type;

      Cur_Subprg : Cursor;
      Subprg : Address_Info_Acc;

      Cur_Symbol : Cursor;
      Symbol : Address_Info_Acc;

      Last_Addr : Pc_Type;
   begin
      Cur := First (File.Desc_Sets (Section_Addresses));

      if not Is_Empty (File.Desc_Sets (Subprogram_Addresses)) then
         Cur_Subprg := First (File.Desc_Sets (Subprogram_Addresses));
         Subprg := Element (Cur_Subprg);
      else
         Subprg := null;
      end if;

      if not Is_Empty (File.Desc_Sets (Symbol_Addresses)) then
         Cur_Symbol := First (File.Desc_Sets (Symbol_Addresses));
         Symbol := Element (Cur_Symbol);
      else
         Symbol := null;
      end if;

      while Cur /= No_Element loop
         Sec := Element (Cur);
         Load_Section_Content (File, Sec);

         --  Display section name

         Put ("Section ");
         Put (Sec.Section_Name.all);
         Put (':');

         if Sec.Section_Name'Length < 16 then
            Put ((1 .. 16 - Sec.Section_Name'Length => ' '));
         end if;

         Put (' ');
         Put (Hex_Image (Sec.First));
         Put ('-');
         Put (Hex_Image (Sec.Last));
         New_Line;

         Addr := Sec.First;
         Last_Addr := Sec.Last;

         --  Search next matching symbol

         while Symbol /= null and then Addr > Symbol.First loop
            Next (Cur_Symbol);
            if Cur_Symbol = No_Element then
               Symbol := null;
               exit;
            end if;
            Symbol := Element (Cur_Symbol);
         end loop;

         --  Iterate on addresses range for this section

         while Addr <= Sec.Last loop
            Last_Addr := Sec.Last;

            --  Look for the next subprogram

            while Subprg /= null and then Addr > Subprg.Last loop
               Next (Cur_Subprg);
               if Cur_Subprg = No_Element then
                  Subprg := null;
                  exit;
               end if;
               Subprg := Element (Cur_Subprg);
            end loop;

            --  Display subprogram name

            if Subprg /= null then
               if Addr = Subprg.First then
                  New_Line;
                  Put ('<');
                  Put (Subprg.Subprogram_Name.all);
                  Put ('>');
               end if;

               if Last_Addr > Subprg.Last then
                  Last_Addr := Subprg.Last;
               end if;
            end if;

            --  Display Symbol

            if Symbol /= null then
               if Addr = Symbol.First
                    and then
                  (Subprg = null or else (Subprg.Subprogram_Name.all
                                            /= Symbol.Symbol_Name.all))
               then
                  Put ('<');
                  Put (Symbol.Symbol_Name.all);
                  Put ('>');
                  if Subprg = null or else Subprg.First /= Addr then
                     Put (':');
                     New_Line;
                  end if;
               end if;

               while Symbol /= null and then Addr >= Symbol.First loop
                  Next (Cur_Symbol);
                  if Cur_Symbol = No_Element then
                     Symbol := null;
                     exit;
                  end if;
                  Symbol := Element (Cur_Symbol);
               end loop;

               if Symbol /= null and then Symbol.First < Last_Addr then
                  Last_Addr := Symbol.First - 1;
               end if;
            end if;

            if Subprg /= null and then Addr = Subprg.First then
               Put (':');
               New_Line;
            end if;

            Traces_Disa.For_Each_Insn
              (Slice (Sec.Section_Content, Addr, Last_Addr),
               Not_Covered, Traces_Disa.Textio_Disassemble_Cb'Access, File);

            Addr := Last_Addr;
            exit when Addr = Pc_Type'Last;
            Addr := Addr + 1;
         end loop;

         Next (Cur);
      end loop;
   end Disassemble_File;

   procedure On_Elf_From
     (Filename : String;
      Cb       : access procedure (Exec : Exe_File_Acc));
   --  Call CB with an open executable file descriptor for FILE,
   --  closed on return.

   -----------------
   -- On_Elf_From --
   -----------------

   procedure On_Elf_From
     (Filename : String;
      Cb       : access procedure (Exec : Exe_File_Acc))
   is
      Exec : Exe_File_Acc;
   begin
      Open_Exec (Filename, 0, Exec); --  ??? Text_Start

      declare
         Efile : Elf_File renames Exec.Exe_File;
      begin
         Load_Shdr (Efile);
         Cb (Exec);
         Close_File (Efile);
      end;
   exception
      when Elf_Files.Error =>
         Put_Line (Standard_Error, "cannot open: " & Filename);
         raise;
   end On_Elf_From;

   -----------------------
   -- Scan_Symbols_From --
   -----------------------

   procedure Scan_Symbols_From
     (File   : Exe_File_Acc;
      Sym_Cb : access procedure (Sym : Address_Info_Acc);
      Strict : Boolean)
   is
      use Address_Info_Sets;

      Efile : Elf_File renames File.Exe_File;
      --  Corresponding Elf_File - as we do low level accesses

      Nbr_Shdr : constant Elf_Half := Get_Shdr_Num (Efile);

      type Set_Acc is access Address_Info_Sets.Set;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Address_Info_Sets.Set, Set_Acc);
      type Set_Acc_Array is array (0 .. Nbr_Shdr) of Set_Acc;

      Shdr_Sets : Set_Acc_Array := (others => null);
      --  Addresses container for each relevant sections

      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Address_Info, Address_Info_Acc);

      Shdr           : Elf_Shdr_Acc;
      Last           : Pc_Type;
      Addr           : Pc_Type;
      Offset         : Pc_Type;

      Sym            : Address_Info_Acc;
      Cur_Sym        : Address_Info_Sets.Cursor;

      Symtab_Len     : Elf_Addr;
      Symtabs        : Binary_Content;
      Symtabs_Region : Mapped_Region;
      Symtab_Base    : Address;

      Strtab_Idx     : Elf_Half;
      Strtab_Len     : Elf_Addr;
      Strtabs        : Binary_Content;
      Strtabs_Region : Mapped_Region;

      A_Sym          : Elf_Sym;
      Sym_Name       : String_Access;

      Sym_Type       : Unsigned_8;
      Cur            : Address_Info_Sets.Cursor;
      Ok             : Boolean;
      Do_Reloc       : Boolean;
   begin
      --  Search symtab and strtab, exit in case of failure

      if File.Sec_Symtab = SHN_UNDEF then
         Put_Line ("# No symbol table - file stripped ?");
         return;
      end if;
      Strtab_Idx := Get_Strtab_Idx (File.all);
      if Strtab_Idx = SHN_UNDEF then
         Put_Line ("# no strtab for .symtab - ill formed ELF file ?");
         return;
      end if;

      --  Build sets for A+X sections
      --  FIXME: this somewhat duplicates the logic of Build_Sections.

      for Idx in 0 .. Nbr_Shdr - 1 loop
         Shdr := Get_Shdr (Efile, Idx);

         --  Only A+X sections are interesting

         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
           and then Shdr.Sh_Size > 0
         then
            Shdr_Sets (Idx) := new Address_Info_Sets.Set;
         end if;
      end loop;

      --  Load symtab and strtab

      Alloc_And_Load_Section (File.all, File.Sec_Symtab,
                              Symtab_Len, Symtabs,
                              Symtabs_Region, Symtab_Base);
      Alloc_And_Load_Section (File.all, Strtab_Idx,
                              Strtab_Len, Strtabs, Strtabs_Region);

      --  Walk the symtab and put interesting symbols into the containers.
      --  Except for warnings in strict mode, leave empty symbols alone. We'd
      --  need to be extra careful with the beginning or end of sections and
      --  we wouldn't be able to analyze much about the symbols anyway.

      --  FIXME: this somewhat duplicates the logic of Build_Symbols.

      for I in 1 .. Natural (Symtab_Len) / Elf_Sym_Size loop
         A_Sym := Get_Sym
           (Efile, Symtab_Base + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (A_Sym.St_Info);

         if  (Sym_Type = STT_FUNC or Sym_Type = STT_NOTYPE)
           and then A_Sym.St_Shndx in Shdr_Sets'Range
           and then Shdr_Sets (A_Sym.St_Shndx) /= null
         then

            Sym_Name := new String'
              (Read_String (Address_Of (Strtabs, Elf_Addr (A_Sym.St_Name))));

            if A_Sym.St_Size = 0 then

               --  Empty symbol. Warn if requested to do so, then just release
               --  what we have allocated for the symbol already.

               if Strict then
                  Put_Line
                    (Standard_Error,
                     "warning: empty symbol " & Sym_Name.all
                       & " at " & Hex_Image (A_Sym.St_Value)
                       & " in section "
                       & Get_Shdr_Name (Efile, A_Sym.St_Shndx));
               end if;

               Free (Sym_Name);

            else

               --  Non-empy symbol. Latch into our local container for
               --  processing downstream.

               Address_Info_Sets.Insert
                 (Shdr_Sets (A_Sym.St_Shndx).all,
                  new Address_Info'
                    (Kind        => Symbol_Addresses,
                     First       => Pc_Type (A_Sym.St_Value),
                     Last        => Pc_Type (A_Sym.St_Value
                                               + Elf_Addr (A_Sym.St_Size) - 1),
                     Parent      => null,
                     Symbol_Name => Sym_Name,
                     others      => <>),
                  Cur, Ok);

               if not Ok then
                  Put_Line (Standard_Error,
                            "symbol " & Sym_Name.all
                              & " is an alias at "
                              & Hex_Image (A_Sym.St_Value));
               end if;

            end if;
         end if;
      end loop;

      Free (Strtabs_Region);
      Free (Symtabs_Region);

      --  Walk the sections, invoking callback or warning for symbols of
      --  interest as we go along.

      Do_Reloc := Get_Ehdr (Efile).E_Type = ET_REL;
      Offset := 0;
      for I in Shdr_Sets'Range loop
         if Shdr_Sets (I) /= null then
            Shdr := Get_Shdr (Efile, I);

            --  Section range

            Addr := Pc_Type (Shdr.Sh_Addr);
            Last := Pc_Type (Shdr.Sh_Addr + Shdr.Sh_Size - 1);

            Cur_Sym := First (Shdr_Sets (I).all);

            Sym := (if Has_Element (Cur_Sym)
                    then Element (Cur_Sym)
                    else null);

            --  Get the first symbol in the section

            if Do_Reloc then
               Offset := Addr;
            end if;
            while Sym /= null and then Sym.First + Offset < Addr loop
               --  Can this happen ?
               Put_Line
                 (Standard_Error, "symbol " & Sym.Symbol_Name.all
                  & " doesn't belong to its section "
                  &  Get_Shdr_Name (Efile, I)
                  & " [" & Unsigned_16'Image (I) & " ]");

               Free (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);

               Next (Cur_Sym);
               Sym := (if Has_Element (Cur_Sym)
                       then Element (Cur_Sym)
                       else null);
            end loop;

            --  Now, process the symbols that are in the section's range.
            --  We expect empty symbols to have been filtered out already.

            while Sym /= null and then Sym.Last + Offset <= Last loop

               pragma Assert (Sym.Last >= Sym.First);

               if Strict and then Sym.First + Offset > Addr then
                  Put_Line
                    (Standard_Error,
                     "warning: no symbols for "
                       & Hex_Image (Addr) & "-"
                       & Hex_Image (Sym.First + Offset - 1)
                       & " in section " &  Get_Shdr_Name (Efile, I)
                       & " [" & Unsigned_16'Image (I) & " ]");
               end if;

               if Sym_Cb /= null then
                  Sym_Cb.all (Sym);
               end if;

               Addr := Sym.Last + Offset;
               exit when Addr = Pc_Type'Last;
               Addr := Addr + 1;

               Free (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);

               Next (Cur_Sym);
               Sym := (if Has_Element (Cur_Sym)
                       then Element (Cur_Sym)
                       else null);
            end loop;

            if Strict and then Addr < Last then
               Put_Line
                 (Standard_Error,
                  "warning: no symbols for "
                  & Hex_Image (Addr) & "-" & Hex_Image (Last)
                  & " in section " &  Get_Shdr_Name (Efile, I)
                  & " [" & Unsigned_16'Image (I) & " ]");
            end if;
            Unchecked_Deallocation (Shdr_Sets (I));
         end if;
      end loop;
   end Scan_Symbols_From;

   -----------------------
   -- Scan_Symbols_From --
   -----------------------

   procedure Scan_Symbols_From
     (Filename : String;
      Sym_Cb   : access procedure (Sym : Address_Info_Acc);
      Strict   : Boolean)
   is
      --  Bridge to the version working from an executable file descriptor

      procedure Process (Exec : Exe_File_Acc);

      procedure Process (Exec : Exe_File_Acc) is
      begin
         Scan_Symbols_From (Exec, Sym_Cb, Strict);
      end Process;
   begin
      On_Elf_From (Filename, Process'Access);
   end Scan_Symbols_From;

   ------------------------
   -- Read_Routine_Names --
   ------------------------

   procedure Read_Routine_Names
     (File    : Exe_File_Acc;
      Exclude : Boolean;
      Strict  : Boolean := False)
   is
      procedure Add_Symbol (Sym : Address_Info_Acc);
      --  Acknowledge one SYMbol from scan on FILE, adding to or
      --  removing from the routines database depending on EXCLUDE.

      ----------------
      -- Add_Symbol --
      ----------------

      procedure Add_Symbol (Sym : Address_Info_Acc) is
      begin
         if Exclude then
            Traces_Names.Remove_Routine_Of_Interest (Sym.Symbol_Name.all);
         else
            --  Read_Routine_Names is called only when the following two
            --  conditions are met:
            --  - there is only one executable
            --  - no list of symbols is provided
            --  In this specific situation (when reading the list of symbols
            --  from the executable), we *have to* avoid adding the same symbol
            --  name twice.
            --
            if not Traces_Names.Is_Routine_Of_Interest
               (Sym.Symbol_Name.all)
            then
               Traces_Names.Add_Routine_Of_Interest (Sym.Symbol_Name.all);
            end if;

            --  Scan_Symbols_From is going to free this instance of Sym, so
            --  take the ownership of the Symbol_Name string.
            --
            Sym.Symbol_Name := null;
         end if;
      end Add_Symbol;

   --  Start of processing for Read_Routine_Names

   begin
      Scan_Symbols_From
        (File   => File,
         Sym_Cb => Add_Symbol'Access,
         Strict => Strict);
   end Read_Routine_Names;

   ------------------------
   -- Read_Routine_Names --
   ------------------------

   procedure Read_Routine_Names
     (Filename : String;
      Exclude  : Boolean;
      Strict   : Boolean)
   is
      --  Wrapper for the version working from an executable file descriptor

      procedure Process (Exec : Exe_File_Acc);
      --  Needs comment???

      -------------
      -- Process --
      -------------

      procedure Process (Exec : Exe_File_Acc) is
      begin
         Read_Routine_Names (Exec, Exclude, Strict);
      end Process;

   --  Start of processing for Read_Routine_Names

   begin
      On_Elf_From (Filename, Process'Access);
   end Read_Routine_Names;

   ------------------------------
   -- Routine_Names_From_Lines --
   ------------------------------

   procedure Routine_Names_From_Lines
     (Exec     : Exe_File_Acc;
      Selected : not null access
                   function (Sloc_Begin : Source_Location;
                             Sloc_End   : Source_Location) return Boolean)
   is
      use Address_Info_Sets;
      use Traces_Names;

      Line_Cursor : Cursor;
      Sym_End_Addr : aliased Address_Info (Line_Addresses);

      Subp_Key : Subprogram_Key;

   begin
      for Subprg of Exec.Desc_Sets (Subprogram_Addresses) loop
         Line_Cursor := First (Subprg.Lines);
         while Has_Element (Line_Cursor) loop
            declare
               Line_Addr        : constant Address_Info_Acc :=
                 Element (Line_Cursor);
               Sloc_Begin       : constant Source_Location := Line_Addr.Sloc;
               Sloc_End         : Source_Location;
               Next_Line_Cursor : Cursor;
               Next_Line_Addr   : Address_Info_Acc;
               Symbol           : Address_Info_Acc;
               Select_Symbol    : Boolean;
            begin
               Symbol := Get_Address_Info (Exec.all,
                                           Symbol_Addresses,
                                           Line_Addr.First);

               --  Because of code elimination, we may have lines that
               --  correspond to no code; in which case Symbol can be null. In
               --  such a case, we just want to skip all lines that that are
               --  associated to this null symbol.

               if Symbol /= null then
                  Next_Line_Cursor := Next (Line_Cursor);

                  if Has_Element (Next_Line_Cursor) then
                     --  Consider line range up to next line info if
                     --  remaining within the same symbol.

                     Next_Line_Addr := Element (Next_Line_Cursor);

                     if Get_Symbol
                       (Exec.all, Next_Line_Addr.First) = Symbol
                     then
                        Sloc_End := Next_Line_Addr.Sloc;
                     else
                        Sloc_End := Sloc_Begin;
                     end if;
                  else
                     Sloc_End := Sloc_Begin;
                  end if;

                  --  Two different cases:
                  --
                  --  * if the two consecutive slocs are in the same source
                  --  file, we check if there is a SCO in this range. Not
                  --  strictly correct: consider the case when a function
                  --  declared in a package is inlined in an other function
                  --  inside this same package; in this case, the range defined
                  --  by two consecutive debug slocs may not correspond to
                  --  anything relevant in the source code. This should not
                  --  matter much though. Inlining causes other problems to
                  --  statement coverage anyway. Plus, the consequence of this
                  --  error will just be to include a routine in a package that
                  --  contains SCO; that's certainly fine as, in the source
                  --  coverage case, the routine list is mostly a way to select
                  --  the source files to handle; if we have some SCOs in
                  --  the file in which a routine is defined, it is certainly
                  --  appropriate to add it to trace name database.
                  --
                  --  * if the two consecutive slocs are in a different source
                  --  file. in this case, it is never a good idea to consider
                  --  the range of these two slocs. Deal with them separately.
                  --
                  --  In any case, the whole last line is included in its range
                  --  by taking the maximum column number.

                  if Sloc_Begin.Source_File = Sloc_End.Source_File
                    and then Sloc_Begin < Sloc_End
                  then
                     Sloc_End.L.Column := Natural'Last;
                     Select_Symbol := Selected (Sloc_Begin, Sloc_End);

                  else
                     declare
                        Sloc_End : Source_Location := Sloc_Begin;
                     begin
                        Sloc_End.L.Column := Natural'Last;
                        Select_Symbol := Selected (Sloc_Begin, Sloc_End);
                     end;

                     declare
                        Sloc_Begin : constant Source_Location := Sloc_End;
                     begin
                        Sloc_End.L.Column := Natural'Last;
                        Select_Symbol :=
                          Select_Symbol
                          or else Selected (Sloc_Begin, Sloc_End);
                     end;
                  end if;

                  --  Now, include the symbol to the routine table if it
                  --  is selected and not already included:

                  if Select_Symbol then

                     --  There can be symbols that have the same name, but that
                     --  are different anyway.

                     if not Is_Routine_Of_Interest
                              (Symbol.Symbol_Name.all)
                     then
                        Add_Routine_Of_Interest (Symbol.Symbol_Name.all);
                     end if;

                     Key_From_Symbol (Exec, Symbol, Subp_Key);
                     if not Is_In (Subp_Key) then
                        Add_Routine (Subp_Key, Exec, Symbol.Symbol_Tag);
                        Symbol.Symbol_Origin := Subp_Key.Origin;
                     end if;

                     --  Fast-forward to end of symbol

                     Sym_End_Addr.First := Symbol.Last;
                     Sym_End_Addr.Last  := Symbol.Last;
                     Line_Cursor :=
                       Subprg.Lines.Ceiling (Sym_End_Addr'Unchecked_Access);

                     exit when not Has_Element (Line_Cursor);
                  end if;
               end if;
            end;
            Next (Line_Cursor);
         end loop;
      end loop;
   end Routine_Names_From_Lines;

   --------------
   -- Relocate --
   --------------

   procedure Relocate
     (Bin_Cont  : in out Binary_Content;
      New_First : Elf_Arch.Elf_Addr) is
   begin
      Bin_Cont.Last := New_First + Length (Bin_Cont) - 1;
      Bin_Cont.First := New_First;
   end Relocate;

   ------------
   -- Length --
   ------------

   function Length (Bin_Cont : Binary_Content) return Elf_Arch.Elf_Addr is
   begin
      if Bin_Cont.First > Bin_Cont.Last then
         return 0;
      else
         return Bin_Cont.Last - Bin_Cont.First + 1;
      end if;
   end Length;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Bin_Cont : Binary_Content) return Boolean is
   begin
      return Bin_Cont.Content /= null;
   end Is_Loaded;

   ---------
   -- Get --
   ---------

   function Get
     (Bin_Cont : Binary_Content;
      Offset : Elf_Arch.Elf_Addr) return Interfaces.Unsigned_8 is
   begin
      return Bin_Cont.Content (Offset - Bin_Cont.First);
   end Get;

   -----------
   -- Slice --
   -----------

   function Slice
     (Bin_Cont    : Binary_Content;
      First, Last : Elf_Arch.Elf_Addr) return Binary_Content
   is
      RFirst : constant Elf_Arch.Elf_Addr :=
        (if Bin_Cont.First <= First
         then First
         else raise Constraint_Error with "First out of bounds");
      RLast : constant Elf_Arch.Elf_Addr :=
        (if Bin_Cont.Last >= Last
         then Last
         else raise Constraint_Error with "Last out of bounds");
   begin
      return
        (Content => Convert (Address_Of (Bin_Cont, RFirst)),
         First   => RFirst,
         Last    => RLast);
   end Slice;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (Bin_Cont : Binary_Content;
      Offset : Elf_Arch.Elf_Addr) return System.Address is
   begin
      return Bin_Cont.Content (Offset - Bin_Cont.First)'Address;
   end Address_Of;

   ------------------
   -- Make_Mutable --
   ------------------

   procedure Make_Mutable
     (Exe      : Exe_File_Type;
      Region   : in out Mapped_Region;
      Bin_Cont : out Binary_Content) is
   begin
      Make_Mutable (Exe.Exe_File, Region);
      Bin_Cont :=
        (Content => Convert (Data (Region)),
         First   => 0,
         Last    => Pc_Type (Last (Region)));
   end Make_Mutable;
end Traces_Elf;
