------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Characters.Handling;
with Interfaces; use Interfaces;

with System.Storage_Elements; use System.Storage_Elements;

with GNAT.OS_Lib;

with Coverage;          use Coverage;
with Coverage.Object;   use Coverage.Object;
with Disa_Common;       use Disa_Common;
with Dwarf;
with Dwarf_Handling;    use Dwarf_Handling;
with Elf32;
with Elf_Disassemblers; use Elf_Disassemblers;
with Execs_Dbase;       use Execs_Dbase;
with Hex_Images;        use Hex_Images;
with Traces_Disa;
with Traces_Lines;      use Traces_Lines;
with Traces_Names;
with Types;             use Types;
with Files_Table;       use Files_Table;
with Outputs;

package body Traces_Elf is

   No_Stmt_List : constant Unsigned_32 := Unsigned_32'Last;
   --  Value indicating there is no AT_stmt_list.

   No_Ranges    : constant Unsigned_32 := Unsigned_32'Last;
   --  Value indicating there is no AT_ranges.

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
   procedure Read_Word4
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Integer_32);
   procedure Read_Word2
     (Exec : Exe_File_Type;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_16);
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
     (Exec    : Exe_File_Type;
      Sec_Rel : Elf_Half;
      Data    : in out Binary_Content);

   procedure Read_Debug_Lines
     (Exec                  : in out Exe_File_Type;
      Stmt_List_Offset      : Unsigned_32;
      Compilation_Directory : String_Access);
   --  Read the debug lines of a compilation unit.
   --  Stmt_List_Offset is the offset of a stmt list from the
   --  beginning of the .debug_line section of Exec; Compilation_Directory
   --  is the value of DW_AT_comp_dir for the compilation unit, or is null
   --  if this attribute is not specified.

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Size;
      Content : out Binary_Content_Acc);
   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Size;
      Content : out Binary_Content_Acc;
      Base    : out Address);
   --  Allocate memory for section SEC of EXEC and read it.
   --  LEN is the length of the section, CONTENT is its binary content.
   --  The low bound of CONTENT is 0.

   function Build_Filename (Dir : String; Filename : String)
                           return String_Access;
   --  Create a filename from a directory name and a filename.
   --  The directory name is expected to be not empty.
   --  If the filename looks like a Windows filename, it is canonicalized.

   Empty_String_Acc : constant String_Access := new String'("");

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Addresses_Info_Acc) return Boolean is
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

   begin
      if L.First < R.First then
         return True;
      elsif R.First < L.First then
         return False;
      end if;

      --  Here if L.First = R.First

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

   function Image (El : Addresses_Info_Acc) return String is
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
              & Sloc_Image (Line => El.Sloc.Line, Column => El.Sloc.Column);
      end case;
   end Image;

   ------------------
   -- Disp_Address --
   ------------------

   procedure Disp_Address (El : Addresses_Info_Acc) is
   begin
      Put_Line (Image (El));
   end Disp_Address;

   --------------------
   -- Disp_Addresses --
   --------------------

   procedure Disp_Addresses (Exe : Exe_File_Type; Kind : Addresses_Kind) is
      use Addresses_Containers;

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
      Exe.Desc_Sets (Kind).Iterate (Disp_Address'Access);
   end Disp_Addresses;

   ----------------------------
   -- Disp_Compilation_Units --
   ----------------------------

   procedure Disp_Compilation_Units (Exec : Exe_File_Type) is
      use Compile_Unit_Lists;
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
     (Set : in out Addresses_Containers.Set;
      El  : Addresses_Info_Acc)
      renames Addresses_Containers.Insert;

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
         raise Program_Error;
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

      Unchecked_Deallocation (Exec.Lines);
      Exec.Lines_Len := 0;

      Unchecked_Deallocation (Exec.Debug_Strs);
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
   -- Read_Word4 --
   ----------------

   procedure Read_Word4 (Exec : Exe_File_Type;
                         Base : Address;
                         Off : in out Storage_Offset;
                         Res : out Integer_32)
   is
      function To_Integer_32 is
        new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);
      R : Unsigned_32;
   begin
      Read_Word4 (Exec, Base, Off, R);
      Res := To_Integer_32 (R);
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
                                          Exec.Debug_Strs);
                  if Exec.Sec_Debug_Str = SHN_UNDEF then
                     return;
                  end if;
                  Exec.Debug_Str_Base := Exec.Debug_Strs (0)'Address;
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
            Put ("???");
            raise Program_Error;
      end case;
   end Skip_Dwarf_Form;

   -----------------------
   -- Apply_Relocations --
   -----------------------

   procedure Apply_Relocations
     (Exec    : Exe_File_Type;
      Sec_Rel : Elf_Half;
      Data    : in out Binary_Content)
   is
      use Elf32;
      Relocs_Len : Elf_Size;
      Relocs : Binary_Content_Acc;
      Relocs_Base : Address;

      Shdr : Elf_Shdr_Acc;
      Off : Storage_Offset;

      R : Elf_Rela;
   begin
      Shdr := Get_Shdr (Exec.Exe_File, Sec_Rel);
      if Shdr.Sh_Type /= SHT_RELA then
         raise Program_Error;
      end if;
      if Natural (Shdr.Sh_Entsize) /= Elf_Rela_Size then
         raise Program_Error;
      end if;
      Alloc_And_Load_Section (Exec, Sec_Rel, Relocs_Len, Relocs, Relocs_Base);

      Off := 0;
      while Off < Storage_Offset (Relocs_Len) loop
         if
           Off + Storage_Offset (Elf_Rela_Size) > Storage_Offset (Relocs_Len)
         then
            --  Truncated

            raise Program_Error;
         end if;

         --  Read relocation entry

         Read_Word4 (Exec, Relocs_Base, Off, R.R_Offset);
         Read_Word4 (Exec, Relocs_Base, Off, R.R_Info);
         Read_Word4 (Exec, Relocs_Base, Off, R.R_Addend);

         if R.R_Offset > Data'Last then
            raise Program_Error;
         end if;

         case Exec.Exe_Machine is
            when EM_PPC =>
               case Elf_R_Type (R.R_Info) is
                  when R_PPC_ADDR32 =>
                     null;
                  when others =>
                     raise Program_Error;
               end case;
            when others =>
               raise Program_Error;
         end case;

         Write_Word4 (Exec,
                      Data (0)'Address,
                      Storage_Offset (R.R_Offset), R.R_Addend);
      end loop;
      Unchecked_Deallocation (Relocs);
   end Apply_Relocations;

   ----------------------------
   -- Alloc_And_Load_Section --
   ----------------------------

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Size;
      Content : out Binary_Content_Acc)
   is
   begin
      if Sec /= 0 then
         Len := Get_Section_Length (Exec.Exe_File, Sec);
         pragma Assert (Len > 0);
         Content := new Binary_Content (0 .. Len - 1);
         Load_Section (Exec.Exe_File, Sec, Content (0)'Address);
      end if;
   end Alloc_And_Load_Section;

   ----------------------------
   -- Alloc_And_Load_Section --
   ----------------------------

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type;
      Sec     : Elf_Half;
      Len     : out Elf_Size;
      Content : out Binary_Content_Acc;
      Base    : out Address)
   is
   begin
      if Sec /= 0 then
         Alloc_And_Load_Section (Exec, Sec, Len, Content);
         Base := Content (0)'Address;
      else
         Content := null;
         Base := Null_Address;
      end if;
   end Alloc_And_Load_Section;

   --  Extract lang, subprogram name and stmt_list (offset in .debug_line).
   --  What does this comment apply to???

   -------------------------------
   -- Build_Debug_Compile_Units --
   -------------------------------

   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type) is
      use Dwarf;

      Abbrev_Len : Elf_Size;
      Abbrevs : Binary_Content_Acc;
      Abbrev_Base : Address;
      Map : Abbrev_Map_Acc;
      Abbrev : Address;

      Info_Len : Elf_Size;
      Infos : Binary_Content_Acc;
      Base : Address;
      Off : Storage_Offset;
      Aoff : Storage_Offset;

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

      At_Sib       : Unsigned_64 := 0;
      At_Stmt_List : Unsigned_32 := No_Stmt_List;
      At_Ranges    : Unsigned_32 := No_Ranges;
      At_Low_Pc    : Unsigned_64 := 0;
      At_High_Pc   : Unsigned_64 := 0;
      At_Lang      : Unsigned_64 := 0;
      At_Name      : Address := Null_Address;
      At_Comp_Dir  : Address := Null_Address;
      Cu_Base_Pc   : Unsigned_64;

      Current_Sec     : Addresses_Info_Acc;
      Current_Subprg  : Addresses_Info_Acc;
      Compilation_Dir : String_Access;
      Unit_Filename   : String_Access;
      Subprg_Low      : Pc_Type;

   begin
      --  Return now if already loaded

      if not Exec.Compile_Units.Is_Empty then
         return;
      end if;

      if Exec.Desc_Sets (Section_Addresses).Is_Empty then
         raise Program_Error;
      end if;

      --  Load .debug_abbrev

      Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Abbrev,
                              Abbrev_Len, Abbrevs, Abbrev_Base);

      Map := null;

      --  Load .debug_info

      Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Info, Info_Len,
                              Infos, Base);

      if Exec.Sec_Debug_Info_Rel /= SHN_UNDEF then
         Apply_Relocations (Exec, Exec.Sec_Debug_Info_Rel, Infos.all);
      end if;

      Off := 0;
      while Off < Storage_Offset (Info_Len) loop
         --  Read .debug_info header:
         --    Length, version, offset in .debug_abbrev, pointer size.

         Read_Word4 (Exec, Base, Off, Len);
         Last := Off + Storage_Offset (Len);
         Read_Word2 (Exec, Base, Off, Ver);
         Read_Word4 (Exec, Base, Off, Abbrev_Off);
         Read_Byte (Base, Off, Ptr_Sz);
         if Ver /= 2 and Ver /= 3 then
            exit;
         end if;
         Level := 0;

         Exec.Addr_Size := Natural (Ptr_Sz);
         Cu_Base_Pc := 0;

         Build_Abbrev_Map (Abbrev_Base + Storage_Offset (Abbrev_Off), Map);

         --  Read DIEs

         loop
            << Again >> null;
            exit when Off >= Last;
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
                  Exec.Compile_Units.Append
                    (Compile_Unit_Desc'(Unit_Filename,
                                        Compilation_Dir,
                                        At_Stmt_List));

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
                     --  Don't care about missing subprograms

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
                       new Addresses_Info'
                       (Kind            => Subprogram_Addresses,
                        First           => Subprg_Low,
                        Last            =>
                          Exec.Exe_Text_Start + Pc_Type (At_High_Pc - 1),
                        Parent          => Current_Sec,
                        Subprogram_Name => new String'(Read_String (At_Name)));
                     Exec.Desc_Sets (Subprogram_Addresses).
                       Insert (Current_Subprg);
                  end if;

               when others =>
                  null;
            end case;
            At_Low_Pc := 0;
            At_High_Pc := 0;
            At_Ranges := No_Ranges;

            At_Name := Null_Address;
            At_Comp_Dir := Null_Address;
         end loop;
         Unchecked_Deallocation (Map);
      end loop;

      Unchecked_Deallocation (Infos);
      Unchecked_Deallocation (Abbrevs);
   end Build_Debug_Compile_Units;

   package Filenames_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => String_Access,
      "=" => "=");

   function Build_Filename
     (Dir : String; Filename : String) return String_Access
   is
      use Ada.Characters.Handling;
      Res : String := Dir & '/' & Filename;
   begin
      if Res'Length > 2 and then Res (Res'First + 1) = ':' then
         --  Looks like a Windows file name

         --  Capitalize the driver letter

         Res (Res'First) := To_Upper (Res (Res'First));

         --  Lower case letters, back-slashify

         for I in Res'First + 2 .. Res'Last loop
            if Is_Upper (Res (I)) then
               Res (I) := To_Lower (Res (I));
            elsif Res (I) = '/' then
               Res (I) := '\';
            end if;
         end loop;
      end if;
      return new String'(Res);
   end Build_Filename;

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

      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Opc_Length_Type, Opc_Length_Acc);

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

      Pc           : Pc_Type;
      Line, Column : Unsigned_32;
      File         : Natural;
      Line_Base2   : Unsigned_32;

      Nbr_Dirnames  : Unsigned_32;
      Nbr_Filenames : Unsigned_32;
      Dirnames      : Filenames_Vectors.Vector;
      Filenames     : Filenames_Vectors.Vector;

      Last_Line     : Addresses_Info_Acc := null;

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

         Last_Line.Last := Exec.Exe_Text_Start + Pc - 1;
         Last_Line := null;
      end Close_Source_Line;

      ---------------------
      -- New_Source_Line --
      ---------------------

      procedure New_Source_Line is
         use Addresses_Containers;
         Pos        : Cursor;
         Inserted   : Boolean;
      begin
         if Last_Line /= null
           and then Last_Line.First = Exec.Exe_Text_Start + Pc then
            --  If the previous line was empty (i.e. last PC = new PC),
            --  drop it; it is useless in the context of xcov.
            --  Use its slot in the line table to record the new line.

            Last_Line.Sloc :=
              (Source_File  => Get_Index_From_Full_Name
                 (Filenames_Vectors.Element (Filenames, File).all),
               Line         => Natural (Line),
               Column       => Natural (Column));

         else
            --  If the previous line was not empty, finalize it and create a
            --  new one.

            Close_Source_Line;

            --  Note: Last and Parent are set by Build_Debug_Lines

            Last_Line :=
              new Addresses_Info'
              (Kind   => Line_Addresses,
               First  => Exec.Exe_Text_Start + Pc,
               Last   => Exec.Exe_Text_Start + Pc,
               Parent => null,
               Sloc   =>
                 (Source_File  => Get_Index_From_Full_Name
                    (Filenames_Vectors.Element (Filenames, File).all),
                  Line         => Natural (Line),
                  Column       => Natural (Column)));

            Exec.Desc_Sets (Line_Addresses).Insert (Last_Line, Pos, Inserted);
         end if;
      end New_Source_Line;

   --  Start of processing for Read_Debug_Lines

   begin
      --  Load .debug_line

      if Exec.Lines = null then
         Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Line,
                                 Exec.Lines_Len, Exec.Lines, Base);
         if Exec.Sec_Debug_Line_Rel /= SHN_UNDEF then
            Apply_Relocations (Exec, Exec.Sec_Debug_Line_Rel, Exec.Lines.all);
         end if;
      end if;

      Off := Storage_Offset (Stmt_List_Offset);
      if Off >= Storage_Offset (Exec.Lines_Len) then
         return;
      end if;

      Base := Exec.Lines (0)'Address;

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

      Pc := 0;
      Line := 1;
      File := 1;
      Column := 0;

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
              and then File_Dir <= Nbr_Dirnames then
               Dir := Filenames_Vectors.Element (Dirnames, Integer (File_Dir));
            elsif Compilation_Directory /= null
              and then not GNAT.OS_Lib.Is_Absolute_Path (Filename) then
               Dir := Compilation_Directory;
            else
               Dir := Empty_String_Acc;
            end if;

            Filenames_Vectors.Append
              (Filenames, Build_Filename (Dir.all, Filename));
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

            --  Extended opcode.

            Read_ULEB128 (Base, Off, Ext_Len);
            Old_Off := Off;
            Read_Byte (Base, Off, Ext_Opc);
            case Ext_Opc is
               when DW_LNE_end_sequence =>
                  Close_Source_Line;
                  --  Initial state.
                  Pc := 0;
                  Line := 1;
                  File := 1;
                  Column := 0;

               when DW_LNE_set_address =>
                  Read_Address
                    (Exec, Base, Off, Elf_Arch.Elf_Addr'Size / 8, Pc);

               when DW_LNE_define_file =>
                  raise Program_Error with "DW_LNE_define_file unhandled";

               when others =>
                  raise Program_Error with "unhandled DW_LNE";
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
                  Read_SLEB128 (Base, Off, Arg);
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

               when DW_LNS_set_prologue_end
                 | DW_LNS_set_epilogue_begin
                 | DW_LNS_set_isa =>
                  null;

               when others =>
                  --  Instruction length.
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

      Unchecked_Deallocation (Opc_Length);
   end Read_Debug_Lines;

   -----------------------
   -- Build_Debug_Lines --
   -----------------------

   procedure Build_Debug_Lines (Exec : in out Exe_File_Type) is
      use Addresses_Containers;

      function Get_Element (Cur : Cursor) return Addresses_Info_Acc;
      --  Return the element designated by Cur or null if cur doesn't
      --  designate an element.

      function Get_Element (Cur : Cursor) return Addresses_Info_Acc is
      begin
         if Cur /= No_Element then
            return Element (Cur);
         else
            return null;
         end if;
      end Get_Element;

      Cur_Cu : Compile_Unit_Lists.Cursor;
      Cur_Subprg : Cursor;
      Cur_Sec : Cursor;
      Cur_Line : Cursor;
      Cu : Compile_Unit_Desc;
      Subprg : Addresses_Info_Acc;
      Sec : Addresses_Info_Acc;
      Line : Addresses_Info_Acc;
   begin
      --  Return now if already loaded

      if not Exec.Desc_Sets (Line_Addresses).Is_Empty then
         return;
      end if;

      --  Be sure compile units are loaded

      Build_Debug_Compile_Units (Exec);

      --  Read all .debug_line

      Cur_Cu := Exec.Compile_Units.First;
      while Compile_Unit_Lists.Has_Element (Cur_Cu) loop
         Cu := Compile_Unit_Lists.Element (Cur_Cu);
         Read_Debug_Lines (Exec, Cu.Stmt_List, Cu.Compilation_Directory);
         Compile_Unit_Lists.Next (Cur_Cu);
      end loop;

      --  Set Last and Parent

      Cur_Line := First (Exec.Desc_Sets (Line_Addresses));

      Cur_Subprg := First (Exec.Desc_Sets (Subprogram_Addresses));
      Subprg := Get_Element (Cur_Subprg);

      Cur_Sec := First (Exec.Desc_Sets (Section_Addresses));
      Sec := Get_Element (Cur_Sec);

      while Cur_Line /= No_Element loop
         Line := Element (Cur_Line);

         --  Be sure Subprg and Sec are correctly set

         while Subprg /= null and then Subprg.Last < Line.First loop
            Next (Cur_Subprg);
            Subprg := Get_Element (Cur_Subprg);
         end loop;

         while Sec /= null and then Sec.Last < Line.First loop
            Next (Cur_Sec);
            Sec := Get_Element (Cur_Sec);
         end loop;

         if Subprg /= null
           and then Line.First >= Subprg.First
           and then Line.Last <= Subprg.Last
         then
            Line.Parent := Subprg;

         elsif Sec /= null
           and then Line.First >= Sec.First
           and then Line.Last <= Sec.Last
         then
            Line.Parent := Sec;

         else
            --  Possible for discarded sections

            Line.Parent := null;
         end if;

         --  Insert into Sloc -> Line info

         Exec.Known_Slocs.Include (Line.Sloc);

         Next (Cur_Line);
      end loop;
   end Build_Debug_Lines;

   ---------------------
   --  Build_Sections --
   ---------------------

   procedure Build_Sections (Exec : in out Exe_File_Type) is
      Shdr : Elf_Shdr_Acc;
      Addr : Pc_Type;
      Last : Pc_Type;
   begin
      --  Return now if already built

      if not Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      --  Iterate over all section headers

      for Idx in 0 .. Get_Shdr_Num (Exec.Exe_File) - 1 loop
         Shdr := Get_Shdr (Exec.Exe_File, Idx);

         --  Only A+X sections are interesting.

         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
           and then Shdr.Sh_Size > 0
         then
            Addr := Pc_Type (Shdr.Sh_Addr + Exec.Exe_Text_Start);
            Last := Pc_Type (Shdr.Sh_Addr + Exec.Exe_Text_Start
                               + Shdr.Sh_Size - 1);

            Insert (Exec.Desc_Sets (Section_Addresses),
                    new Addresses_Info'
                    (Kind            => Section_Addresses,
                     First           => Addr,
                     Last            => Last,
                     Parent          => null,
                     Section_Name    => new String'(
                                          Get_Shdr_Name (Exec.Exe_File, Idx)),
                     Section_Index   => Idx,
                     Section_Content => null));
         end if;
      end loop;
   end Build_Sections;

   --------------
   -- Get_Sloc --
   --------------

   function Get_Sloc
     (Exec : Exe_File_Type;
      PC   : Pc_Type) return Source_Location
   is
      use Sloc_Sets;

      Line_Info_Before : constant Addresses_Info_Acc :=
                           Get_Address_Info (Exec, Line_Addresses, PC);
   begin
      if Line_Info_Before = null then
         return Slocs.No_Location;
      else
         return Line_Info_Before.Sloc;
      end if;
   end Get_Sloc;

   --------------------------
   -- Load_Section_Content --
   --------------------------

   procedure Load_Section_Content
     (Exec : Exe_File_Type;
      Sec  : Addresses_Info_Acc)
   is
   begin
      if Sec.Section_Content = null then
         Sec.Section_Content := new Binary_Content (Sec.First .. Sec.Last);
         Load_Section (Exec.Exe_File, Sec.Section_Index,
                       Sec.Section_Content (Sec.First)'Address);
      end if;
   end Load_Section_Content;

   --------------------------
   -- Load_Code_And_Traces --
   --------------------------

   procedure Load_Code_And_Traces
     (Exec : Exe_File_Acc;
      Base : access Traces_Base)
   is
      use Addresses_Containers;

      Cur : Cursor;
      Sym : Addresses_Info_Acc;
      Sec : Addresses_Info_Acc;

   begin
      if Is_Empty (Exec.Desc_Sets (Symbol_Addresses)) then
         return;
      end if;

      --  Iterate on symbols

      Cur := Exec.Desc_Sets (Symbol_Addresses).First;
      while Cur /= No_Element loop
         Sym := Element (Cur);

         --  Be sure the section is loaded

         Sec := Sym.Parent;
         Load_Section_Content (Exec.all, Sec);

         --  Add the code and trace information to the symbol's entry in the
         --  routines database.

         begin
            Traces_Names.Add_Code_And_Traces
              (Sym.Symbol_Name,
               Exec,
               Sec.Section_Content (Sym.First .. Sym.Last),
               Base);
         exception
            when others =>
               Disp_Address (Sym);
               raise;
         end;

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
      use Addresses_Containers;

      Cur : Cursor;
      Line : Addresses_Info_Acc;
      Source_File : Source_File_Index := No_Source_File;

      Debug : constant Boolean := False;
      Init_Line_State : Line_State;

   begin
      --  Iterate on lines

      Cur := First (Exec.Desc_Sets (Line_Addresses));
      while Cur /= No_Element loop
         Line := Element (Cur);

         --  Only add lines that are in Section

         exit when Line.Last > Section'Last;
         if Line.First >= Section'First then

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

            Add_Line_For_Object_Coverage
              (Source_File, Init_Line_State,
               Line.Sloc.Line, Line, Base, Exec);

            if Debug then
               New_Line;
               Disp_Address (Line);
            end if;
         end if;

         Next (Cur);
      end loop;
   end Build_Source_Lines_For_Section;

   --------------------
   -- Set_Insn_State --
   --------------------

   procedure Set_Insn_State
     (Base : in out Traces_Base; Section : Binary_Content)
   is
      use Addresses_Containers;

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

      It : Entry_Iterator;
      Trace : Trace_Entry;

   --  Start of processing for Set_Insn_State

   begin
      Init_Post (Base, It, Section'First);
      Get_Next_Trace (Trace, It);

      while Trace /= Bad_Trace loop
         exit when Trace.First > Section'Last;

         case Machine is
            when EM_386 =>
               declare
                  Last_Pc : Pc_Type;
                  Next_Pc : Pc_Type;
                  Len : Natural;

                  New_State : Insn_State;
               begin

                  --  First search the last instruction.

                  Last_Pc := Trace.First;
                  loop
                     Len := Disa_For_Machine (Machine).Get_Insn_Length
                       (Section (Last_Pc .. Trace.Last));
                     Next_Pc := Last_Pc + Pc_Type (Len);
                     exit when Next_Pc = Trace.Last + 1;

                     --  Crash if something got wrong...

                     if Next_Pc > Trace.Last then
                        raise Program_Error;
                     end if;

                     Last_Pc := Next_Pc;
                  end loop;

                  if Section (Last_Pc) in 16#70# .. 16#7F#
                    or else (Section (Last_Pc) = 16#0F#
                             and then
                             Section (Last_Pc + 1) in 16#80# .. 16#8F#)
                  then
                     --  Jcc
                     if Last_Pc > Trace.First then
                        Split_Trace (Base, It, Last_Pc - 1,
                                     Coverage_State (Covered));
                     end if;
                     case Trace.Op and 3 is
                        when 1 =>
                           New_State := Branch_Taken;
                        when 2 =>
                           New_State := Fallthrough_Taken;
                        when 3 =>
                           New_State := Both_Taken;
                        when others =>
                           --  The last instruction is a branch; the trace
                           --  should say if it has been taken or not.
                           --  So Trace.Op shall not be null.
                           --  If the case of instruction coverage, we may
                           --  however ignore this error.
                           if Enabled (Insn) then
                              New_State := Covered;
                           else
                              Outputs.Fatal_Error
                                ("trace at PC=" & Hex_Image (Last_Pc)
                                 & ": incomplete branch coverage information");
                           end if;
                     end case;
                  else
                     New_State := Covered;
                  end if;
                  Update_State (Base, It, Coverage_State (New_State));
               end;

            when EM_PPC =>
               declare
                  procedure Update_Or_Split (Next_State : Insn_State);

                  Insn_Bin : Binary_Content renames Section (Trace.Last - 3
                                                          .. Trace.Last);

                  Branch     : Branch_Kind;
                  Flag_Indir : Boolean;
                  Flag_Cond  : Boolean;
                  Dest       : Pc_Type;

                  Op : constant Unsigned_8 := Trace.Op and 3;
                  Trace_Len : constant Pc_Type := Trace.Last - Trace.First + 1;

                  ---------------------
                  -- Update_Or_Split --
                  ---------------------

                  procedure Update_Or_Split (Next_State : Insn_State) is
                  begin
                     if Trace_Len > 4 then
                        Split_Trace (Base, It, Trace.Last - 4,
                                     Coverage_State (Covered));
                     end if;
                     Update_State (Base, It, Coverage_State (Next_State));
                  end Update_Or_Split;

               begin
                  --  Instructions length is 4

                  if Trace_Len < 4 then
                     raise Program_Error;
                  end if;

                  Disa_For_Machine (Machine).Get_Insn_Properties
                    (Insn_Bin   => Insn_Bin,
                     Pc         => Insn_Bin'First, -- ???
                     Branch     => Branch,
                     Flag_Indir => Flag_Indir,
                     Flag_Cond  => Flag_Cond,
                     Dest       => Dest);

                  if Flag_Cond then
                     case Op is
                        when 1 =>
                           Update_Or_Split (Branch_Taken);
                        when 2 =>
                           Update_Or_Split (Fallthrough_Taken);
                        when 3 =>
                           Update_Or_Split (Both_Taken);
                        when others =>
                           raise Program_Error;
                     end case;

                  else
                     --  Any other case than a conditional branch:
                     --  * either a unconditional
                     --  branch (Opc = 18: b, ba, bl and bla);
                     --  * or a branch conditional with BO=1x1xx
                     --  (branch always);
                     --  * or not a branch. This last case
                     --  may happen when a trace entry has been
                     --  split; in such a case, the ???.
                     Update_State (Base, It, Coverage_State (Covered));
                  end if;
               end;

            when EM_SPARC =>
               declare
                  Op : constant Unsigned_8 := Trace.Op and 3;
                  Pc1 : Pc_Type;
                  Trace_Len : constant Pc_Type := Trace.Last - Trace.First + 1;
                  Nstate : Insn_State;

                  type Br_Kind is (Br_None,
                                   Br_Cond, Br_Cond_A,
                                   Br_Trap, Br_Call, Br_Jmpl, Br_Rett);

                  function Get_Br (Insn : Unsigned_32) return Br_Kind;
                  --  Needs comment???

                  Br1, Br2, Br : Br_Kind;

                  ------------
                  -- Get_Br --
                  ------------

                  function Get_Br (Insn : Unsigned_32) return Br_Kind is
                  begin
                     case Shift_Right (Insn, 30) is
                        when 0 =>
                           case Shift_Right (Insn, 22) and 7 is
                              when 2#010# | 2#110# | 2#111# =>
                                 if (Shift_Right (Insn, 29) and 1) = 0 then
                                    return Br_Cond;
                                 else
                                    return Br_Cond_A;
                                 end if;

                              when others =>
                                 return Br_None;
                           end case;

                        when 1 =>
                           return Br_Call;

                        when 2 =>
                           case Shift_Right (Insn, 19) and 2#111_111# is
                              when 2#111000# =>
                                 return Br_Jmpl;

                              when 2#111001# =>
                                 return Br_Rett;

                              when 2#111_010# =>
                                 return Br_Trap;

                              when others =>
                                 return Br_None;
                           end case;

                        when others =>
                           return Br_None;
                     end case;
                  end Get_Br;

               begin
                  --  Instructions length is 4

                  if Trace_Len < 4 then
                     raise Program_Error;
                  end if;

                  --  Extract last two instructions

                  if Trace_Len > 7 then
                     Br1 := Get_Br
                              (To_Big_Endian_U32 (Section (Trace.Last - 7
                                                        .. Trace.Last - 4)));
                  else
                     Br1 := Br_None;
                  end if;

                  Br2 := Get_Br
                           (To_Big_Endian_U32 (Section (Trace.Last - 3
                                                     .. Trace.Last)));

                  --  Code until the first branch is covered

                  if Br1 = Br_None then
                     Pc1 := Trace.Last - 4;
                     Br := Br2;
                  else
                     Pc1 := Trace.Last - 8;
                     Br := Br1;
                  end if;

                  if Pc1 + 1 > Trace.First then
                     Split_Trace (Base, It, Pc1, Coverage_State (Covered));
                  end if;

                  case Br is
                     when Br_Cond | Br_Cond_A =>
                        case Op is
                           when 0 => Nstate := Covered;
                           when 1 => Nstate := Branch_Taken;
                           when 2 => Nstate := Fallthrough_Taken;
                           when 3 => Nstate := Both_Taken;

                           when others =>
                              raise Program_Error;

                        end case;

                     when Br_None | Br_Call | Br_Trap | Br_Jmpl | Br_Rett =>
                        Nstate := Covered;
                  end case;

                  --  Branch instruction state

                  if Br1 = Br_None then
                     Update_State (Base, It, Coverage_State (Nstate));

                  else
                     Split_Trace (Base, It, Pc1 + 4, Coverage_State (Nstate));

                     --  FIXME: is it sure???
                     Update_State (Base, It, Coverage_State (Covered));
                  end if;
               end;

            when others =>
               exit;
         end case;

         Get_Next_Trace (Trace, It);
      end loop;
   end Set_Insn_State;

   -------------------
   -- Build_Symbols --
   -------------------

   procedure Build_Symbols (Exec : Exe_File_Acc) is
      use Addresses_Containers;

      type Addr_Info_Acc_Arr is array (0 .. Get_Shdr_Num (Exec.Exe_File))
        of Addresses_Info_Acc;
      Sections_Info : Addr_Info_Acc_Arr := (others => null);
      Sec : Addresses_Info_Acc;

      Symtab_Len : Elf_Size;
      Symtabs : Binary_Content_Acc;

      Strtab_Idx : Elf_Half;
      Strtab_Len : Elf_Size;
      Strtabs : Binary_Content_Acc;
      ESym : Elf_Sym;

      Sym_Type : Unsigned_8;
      Sym      : Addresses_Info_Acc;

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

      Cur := First (Exec.Desc_Sets (Section_Addresses));
      while Has_Element (Cur) loop
         Sec := Element (Cur);
         Sections_Info (Sec.Section_Index) := Sec;
         Next (Cur);
      end loop;

      if Exec.Sec_Symtab = SHN_UNDEF then
         return;
      end if;

      Strtab_Idx := Get_Strtab_Idx (Exec.all);
      if Strtab_Idx = SHN_UNDEF then
         return;
      end if;

      Alloc_And_Load_Section (Exec.all, Exec.Sec_Symtab, Symtab_Len, Symtabs);
      Alloc_And_Load_Section (Exec.all, Strtab_Idx, Strtab_Len, Strtabs);

      for I in 1 .. Natural (Symtab_Len) / Elf_Sym_Size loop
         ESym := Get_Sym
           (Exec.Exe_File,
            Symtabs (0)'Address + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (ESym.St_Info);

         if  (Sym_Type = STT_FUNC or else Sym_Type = STT_NOTYPE)
           and then ESym.St_Shndx in Sections_Info'Range
           and then Sections_Info (ESym.St_Shndx) /= null
           and then ESym.St_Size > 0
         then
            Sym := new Addresses_Info'
              (Kind        => Symbol_Addresses,
               First       => Exec.Exe_Text_Start + Pc_Type (ESym.St_Value),
               Last        =>
                 Exec.Exe_Text_Start + Pc_Type (ESym.St_Value
                                                         + ESym.St_Size - 1),
               Parent      => Sections_Info (ESym.St_Shndx),
               Symbol_Name => new String'
                                (Read_String
                                   (Strtabs (ESym.St_Name)'Address)));

            Addresses_Containers.Insert
              (Exec.Desc_Sets (Symbol_Addresses), Sym, Cur, Ok);
         end if;
      end loop;

      Unchecked_Deallocation (Strtabs);
      Unchecked_Deallocation (Symtabs);
   end Build_Symbols;

   ----------------------
   -- Get_Address_Info --
   ----------------------

   function Get_Address_Info
     (Exec : Exe_File_Type;
      Kind : Addresses_Kind;
      PC   : Pc_Type) return Addresses_Info_Acc
   is
      use Addresses_Containers;
      Cur      : Cursor;

      PC_Addr  : aliased Addresses_Info (Kind);
   begin
      PC_Addr.First := PC;
      PC_Addr.Last  := PC;

      --  Note: we assume that type Addresses_Info provides adequate default
      --  initialization so that setting First and Last only yields an element
      --  that is smaller than any element with the same PC and non-default
      --  values for other fields (use of Floor below).

      Cur := Exec.Desc_Sets (Kind).Floor (PC_Addr'Unchecked_Access);
      if Cur = No_Element or else Element (Cur).Last < PC then
         return null;
      else
         return Element (Cur);
      end if;
   end Get_Address_Info;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Exec : Exe_File_Type;
      PC   : Pc_Type) return Addresses_Info_Acc
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

      Symbol : constant Addresses_Info_Acc := Get_Symbol (Sym, Pc);

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
      Kind : Addresses_Kind;
      It   : out Addresses_Iterator)
   is
      use Addresses_Containers;
   begin
      It.Cur := Exe.Desc_Sets (Kind).First;
   end Init_Iterator;

   -------------------
   -- Next_Iterator --
   -------------------

   procedure Next_Iterator
     (It : in out Addresses_Iterator; Addr : out Addresses_Info_Acc)
   is
      use Addresses_Containers;
   begin
      if It.Cur = No_Element then
         Addr := null;
      else
         Addr := Element (It.Cur);
         Next (It.Cur);
      end if;
   end Next_Iterator;

   ------------------------
   -- Read_Routines_Name --
   ------------------------

   procedure Read_Routines_Name
     (Filename  : String;
      Exclude   : Boolean;
      Keep_Open : Boolean)
   is
      Exec : Exe_File_Acc;
   begin
      Open_Exec (Filename, Exec);

      declare
         Efile : Elf_File renames Exec.Exe_File;
      begin
         Load_Shdr (Efile);
         Read_Routines_Name (Exec, Exclude => Exclude);
         if not Keep_Open then
            Close_File (Efile);
         end if;
      end;
   exception
      when Elf_Files.Error =>
         Put_Line (Standard_Error, "cannot open: " & Filename);
         raise;
   end Read_Routines_Name;

   ------------------------
   -- Build_Source_Lines --
   ------------------------

   procedure Build_Source_Lines is
      use Traces_Names;

      procedure Build_Source_Lines_For_Routine
        (Name : String_Access;
         Info : in out Subprogram_Info);
      --  Build source line information from debug information for the given
      --  routine.

      ------------------------------------
      -- Build_Source_Lines_For_Routine --
      ------------------------------------

      procedure Build_Source_Lines_For_Routine
        (Name : String_Access;
         Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Name);
      begin
         if Info.Exec /= null and then Info.Insns /= null then
            Build_Debug_Lines (Info.Exec.all);
            Build_Source_Lines_For_Section
              (Info.Exec, Info.Traces, Info.Insns.all);
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
        (Name : String_Access;
         Info : in out Subprogram_Info);
      --  Set trace state for the given routine

      ------------------------------
      -- Build_Routine_Insn_State --
      ------------------------------

      procedure Build_Routine_Insn_State
        (Name : String_Access;
         Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Name);
      begin
         if Info.Insns /= null then
            Set_Insn_State (Info.Traces.all, Info.Insns.all);
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
      use Addresses_Containers;

      procedure Local_Disassembler (Cur : Cursor);
      --  Comment needed???

      ------------------------
      -- Local_Disassembler --
      ------------------------

      procedure Local_Disassembler (Cur : Cursor) is
         Pc       : Pc_Type;
         Insn_Len : Natural := 0;
         Sec      : constant Addresses_Info_Acc := Element (Cur);
         Insns    : Binary_Content_Acc;
         Line_Pos : Natural;
         Line     : String (1 .. 128);
      begin
         Load_Section_Content (File, Sec);
         Put_Line ("section " & Sec.Section_Name.all);
         Insns := Sec.Section_Content;
         Pc := Insns'First;

         while Pc <= Insns'Last loop
            Put (Hex_Image (Pc));
            Put (":");
            Put (ASCII.HT);

            Disa_For_Machine (Machine).
              Disassemble_Insn (Insns (Pc .. Insns'Last), Pc,
                                Line, Line_Pos, Insn_Len, File);

            for I in Pc .. Pc + Pc_Type (Insn_Len - 1) loop
               Put (Hex_Image (Insns (I)));
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
      use Addresses_Containers;
      Cur : Cursor;
      Sec : Addresses_Info_Acc;
      Addr : Pc_Type;

      Cur_Subprg : Cursor;
      Subprg : Addresses_Info_Acc;

      Cur_Symbol : Cursor;
      Symbol : Addresses_Info_Acc;

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
              (Sec.Section_Content (Addr .. Last_Addr),
               Not_Covered, Traces_Disa.Textio_Disassemble_Cb'Access, File);

            Addr := Last_Addr;
            exit when Addr = Pc_Type'Last;
            Addr := Addr + 1;
         end loop;

         Next (Cur);
      end loop;
   end Disassemble_File;

   ------------------------
   -- Read_Routines_Name --
   ------------------------

   procedure Read_Routines_Name (File : Exe_File_Acc; Exclude : Boolean)
   is
      use Addresses_Containers;
      use Traces_Names;

      Efile : Elf_File renames File.Exe_File;
      --  Corresponding Elf_File - as we do low level accesses

      Nbr_Shdr : constant Elf_Half := Get_Shdr_Num (Efile);

      type Set_Acc is access Addresses_Containers.Set;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Addresses_Containers.Set, Set_Acc);
      type Set_Acc_Array is array (0 .. Nbr_Shdr) of Set_Acc;

      Shdr_Sets : Set_Acc_Array := (others => null);
      --  Addresses container for each relevant sections

      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Addresses_Info, Addresses_Info_Acc);

      Shdr : Elf_Shdr_Acc;
      Last : Pc_Type;
      Addr : Pc_Type;

      Sym : Addresses_Info_Acc;
      Cur_Sym : Addresses_Containers.Cursor;

      Symtab_Len : Elf_Size;
      Symtabs : Binary_Content_Acc;
      Symtab_Base : Address;

      Strtab_Idx : Elf_Half;
      Strtab_Len : Elf_Size;
      Strtabs : Binary_Content_Acc;

      A_Sym : Elf_Sym;
      Sym_Name : String_Access;

      Sym_Type : Unsigned_8;
      Cur : Addresses_Containers.Cursor;
      Ok : Boolean;

      Verbose : constant Boolean := False;
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

      for Idx in 0 .. Nbr_Shdr - 1 loop
         Shdr := Get_Shdr (Efile, Idx);

         --  Only A+X sections are interesting

         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then (Shdr.Sh_Type = SHT_PROGBITS)
           and then Shdr.Sh_Size > 0
         then
            Shdr_Sets (Idx) := new Addresses_Containers.Set;
         end if;
      end loop;

      --  Load symtab and strtab

      Alloc_And_Load_Section (File.all, File.Sec_Symtab, Symtab_Len,
                              Symtabs, Symtab_Base);
      Alloc_And_Load_Section (File.all, Strtab_Idx, Strtab_Len, Strtabs);

      --  Walk the symtab and put interesting symbols into the containers

      for I in 1 .. Natural (Symtab_Len) / Elf_Sym_Size loop
         A_Sym := Get_Sym
           (Efile, Symtab_Base + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (A_Sym.St_Info);

         if  (Sym_Type = STT_FUNC or Sym_Type = STT_NOTYPE)
           and then A_Sym.St_Shndx in Shdr_Sets'Range
           and then Shdr_Sets (A_Sym.St_Shndx) /= null
         then
            Sym_Name := new String'
              (Read_String (Strtabs (A_Sym.St_Name)'Address));

            Addresses_Containers.Insert
              (Shdr_Sets (A_Sym.St_Shndx).all,
               new Addresses_Info'
                 (Kind        => Symbol_Addresses,
                  First       => Pc_Type (A_Sym.St_Value),
                  Last        => Pc_Type (A_Sym.St_Value + A_Sym.St_Size - 1),
                  Parent      => null,
                  Symbol_Name => Sym_Name),
               Cur, Ok);

            if not Ok then
               Put_Line (Standard_Error,
                         "symbol " & Sym_Name.all
                           & " is an alias at " & Hex_Image (A_Sym.St_Value));
            end if;
         end if;
      end loop;

      Unchecked_Deallocation (Strtabs);
      Unchecked_Deallocation (Symtabs);

      --  Walk the sections and put routines into the database

      for I in Shdr_Sets'Range loop
         if Shdr_Sets (I) /= null then
            Shdr := Get_Shdr (Efile, I);

            Addr := Pc_Type (Shdr.Sh_Addr);
            Last := Pc_Type (Shdr.Sh_Addr + Shdr.Sh_Size - 1);

            if Verbose then
               Put_Line ("# " & Hex_Image (Addr) & "-" & Hex_Image (Last)
                           & ": " & Get_Shdr_Name (Efile, I));
            end if;

            Cur_Sym := First (Shdr_Sets (I).all);
            if Has_Element (Cur_Sym) then
               Sym := Element (Cur_Sym);
            else
               Sym := null;
            end if;

            --  Get the first symbol in the section

            while Sym /= null and then Sym.First < Addr loop
               Free (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);
               Next (Cur_Sym);
               if not Has_Element (Cur_Sym) then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            while Sym /= null and then Sym.Last <= Last loop
               if Sym.First > Sym.Last then
                  if Sym.First <= Last then
                     Put_Line
                       (Standard_Error, "empty symbol " & Sym.Symbol_Name.all
                          & " at " & Hex_Image (Sym.First));
                  end if;

               else
                  if Sym.First > Addr then
                     Put_Line
                       (Standard_Error, "no symbols for "
                        & Hex_Image (Addr) & "-" & Hex_Image (Sym.First - 1));
                  end if;

                  if Exclude then
                     Remove_Routine_Name (Sym.Symbol_Name);
                  else
                     begin
                        Add_Routine_Name (Name => Sym.Symbol_Name,
                                          Exec => File);
                        Sym.Symbol_Name := null;
                     exception
                        when Constraint_Error =>
                           --  TODO: improve error message???
                           Put_Line (Standard_Error,
                                     "symbol " & Sym.Symbol_Name.all
                                       & " is defined twice in " &
                                       Get_Filename (Efile));
                     end;
                  end if;

                  Addr := Sym.Last;
                  exit when Addr = Pc_Type'Last;
                  Addr := Addr + 1;
               end if;

               Free (Sym.Symbol_Name);
               Unchecked_Deallocation (Sym);
               Next (Cur_Sym);
               if not Has_Element (Cur_Sym) then
                  Sym := null;
                  exit;
               end if;
               Sym := Element (Cur_Sym);
            end loop;

            if Addr < Last then
               Put_Line
                 (Standard_Error, "no symbols for "
                    & Hex_Image (Addr) & "-" & Hex_Image (Last));
            end if;
            Unchecked_Deallocation (Shdr_Sets (I));
         end if;
      end loop;
   end Read_Routines_Name;

   ------------------------------
   -- Routine_Names_From_Lines --
   ------------------------------

   procedure Routine_Names_From_Lines
     (Exec     : Exe_File_Acc;
      Selected : not null access
                   function (Sloc_Begin : Source_Location;
                             Sloc_End   : Source_Location) return Boolean)
   is
      use Addresses_Containers;
      use Traces_Names;

      Line_Table  : Set renames Exec.Desc_Sets (Line_Addresses);
      Line_Cursor : Cursor;

      procedure Skip_Symbol (Symbol_Addr : Addresses_Info_Acc);
      --  Skip Symbol in the line table; i.e change Line_Cursor to point to
      --  the last line of this symbol.

      -----------------
      -- Skip_Symbol --
      -----------------

      procedure Skip_Symbol
        (Symbol_Addr : Addresses_Info_Acc)
      is
         Line_Addr : Addresses_Info_Acc :=
           new Addresses_Info'(Kind   => Line_Addresses,
                               First  => Symbol_Addr.Last,
                               Last   => Symbol_Addr.Last,
                               Parent => null,
                               Sloc   => Slocs.No_Location);
      begin
         Line_Cursor := Floor (Line_Table, Line_Addr);
         Unchecked_Deallocation (Line_Addr);
      end Skip_Symbol;

   --  Start of processing for Routine_Names_From_Lines

   begin
      Line_Cursor := First (Line_Table);
      while Has_Element (Line_Cursor) loop
         declare
            Line_Addr        : constant Addresses_Info_Acc :=
                                  Element (Line_Cursor);
            Sloc_Begin       : constant Source_Location := Line_Addr.Sloc;
            Next_Line_Cursor : Cursor;
            Next_Line_Addr   : Addresses_Info_Acc;
            Sloc_End         : Source_Location;
            Symbol           : Addresses_Info_Acc;
            Select_Symbol    : Boolean;
         begin
            Next_Line_Cursor := Next (Line_Cursor);
            if Has_Element (Next_Line_Cursor) then
               Next_Line_Addr := Element (Next_Line_Cursor);
               Sloc_End := Next_Line_Addr.Sloc;
            else
               Sloc_End := Sloc_Begin;
            end if;

            --  Two different cases:
            --
            --  * if the two consecutive slocs are in the same source file,
            --  we check if there is a SCO in this range. Not strictly correct:
            --  consider the case when a function declared in a package is
            --  inlined in an other function inside this same package; in
            --  this case, the range defined by two consecutive debug slocs may
            --  not correspond to anything relevant in the source code. This
            --  should not matter much though. Inlining causes other problems
            --  to statement coverage anyway. Plus, the consequence of this
            --  error will just be to include a routine in a package that
            --  contains SCO; that's certainly fine as, in the source coverage
            --  case, the routine list is mostly a way to select the source
            --  files to handle; if we have some SCOs in the file in which
            --  a routine is defined, it is certainly appropriate to add it to
            --  trace name database.
            --
            --  * if the two consecutive slocs are in a different source file.
            --  in this case, it is never a good idea to consider the range of
            --  these two slocs. Deal with them separately.
            --
            --  In any case, the whole last line is included in its range by
            --  taking the maximum column number.

            if Sloc_Begin.Source_File = Sloc_End.Source_File
              and then Sloc_Begin < Sloc_End
            then
               Sloc_End.Column := Natural'Last;
               Select_Symbol := Selected (Sloc_Begin, Sloc_End);

            else
               declare
                  Sloc_End : Source_Location := Sloc_Begin;
               begin
                  Sloc_End.Column := Natural'Last;
                  Select_Symbol := Selected (Sloc_Begin, Sloc_End);
               end;

               declare
                  Sloc_Begin : constant Source_Location := Sloc_End;
               begin
                  Sloc_End.Column := Natural'Last;
                  Select_Symbol :=
                    Select_Symbol or else Selected (Sloc_Begin, Sloc_End);
               end;
            end if;

            --  Now, include the symbol to the routine table if it
            --  is selected and not already included:

            if Select_Symbol then
               Symbol := Get_Address_Info (Exec.all,
                                           Symbol_Addresses,
                                           Line_Addr.First);

               if not Is_In (Symbol.Symbol_Name) then
                  Add_Routine_Name (Symbol.Symbol_Name, Exec);
               end if;

               Skip_Symbol (Symbol);
               exit when not Has_Element (Line_Cursor);
            end if;
         end;
         Next (Line_Cursor);
      end loop;
   end Routine_Names_From_Lines;

end Traces_Elf;
