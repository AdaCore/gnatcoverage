------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

with System.Storage_Elements; use System.Storage_Elements;

with GNATCOLL.Utils; use GNATCOLL.Utils;

with Coff;
with Coverage_Options; use Coverage_Options;
with Coverage;         use Coverage;
with Coverage.Object;  use Coverage.Object;
with Coverage.Source;
with Coverage.Tags;    use Coverage.Tags;
with Diagnostics;
with Disa_Ppc;
with Disassemblers;    use Disassemblers;
with Dwarf;
with Dwarf_Handling;   use Dwarf_Handling;
with Execs_Dbase;      use Execs_Dbase;
with Files_Table;      use Files_Table;
with Hex_Images;       use Hex_Images;
with Inputs;
with Outputs;
with Paths;            use Paths;
with Perf_Counters;    use Perf_Counters;
with Traces_Disa;
with Traces_Lines;     use Traces_Lines;
with Traces_Names;
with Types;            use Types;

package body Traces_Elf is

   procedure Free is new Ada.Unchecked_Deallocation
     (Exe_File_Type'Class, Exe_File_Acc);

   No_Stmt_List : constant Unsigned_32 := Unsigned_32'Last;
   --  Value indicating there is no AT_stmt_list

   type Mapping_Symbol is record
      Address  : Pc_Type;
      Insn_Set : Insn_Set_Type;
   end record;
   --  Temporary data structure created when reading mapping symbols (in ARM
   --  ELF, these tell us whether code is ARM or Thumb). We use these symbols
   --  to fill Insn_Set_Ranges data structures.

   function "<" (L, R : Mapping_Symbol) return Boolean is
     (L.Address < R.Address);

   package Mapping_Symbol_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Mapping_Symbol);

   procedure Build_Insn_Set_Ranges
     (Exec            : in out Exe_File_Type'Class;
      Mapping_Symbols : Mapping_Symbol_Sets.Set;
      Section         : Address_Info_Acc);
   --  Turn Mapping_Symbol_Vectors in to the Insn_Set_Range corresponding to
   --  Section_Index in Exec.

   function Get_Strtab_Idx (Exec : Elf_Exe_File_Type) return Elf_Half;
   --  Get the section index of the symtab string table.
   --  Return SHN_UNDEF if not found (or in case of error).

   procedure Read_Word8
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_64);
   procedure Read_Word4
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_32);
   procedure Read_Word2
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Res  : out Unsigned_16);
   procedure Write_Word8
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_64);
   procedure Write_Word4
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Unsigned_32);
   procedure Write_Word4
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Val  : Integer_32);
   pragma Unreferenced (Write_Word4);
   procedure Read_Address
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Sz   : Natural;
      Res  : out Pc_Type);
   procedure Read_Dwarf_Form_U64
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_64);
   procedure Read_Dwarf_Form_U32
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_32);
   procedure Read_Dwarf_Form_String
     (Exec : in out Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Address);
   procedure Skip_Dwarf_Form
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32);

   procedure Read_Debug_Lines
     (Exec                  : in out Exe_File_Type'Class;
      Stmt_List_Offset      : Unsigned_32;
      Compilation_Directory : String_Access);
   --  Read the debug lines of a compilation unit.
   --
   --  Stmt_List_Offset is the offset of a stmt list from the beginning of the
   --  .debug_line section of Exec; Compilation_Directory is the value of
   --  DW_AT_comp_dir for the compilation unit, or null if this attribute is
   --  not specified.

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type'Class;
      Sec     : Section_Index;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      LS      : out Loaded_Section);
   --  Allocate memory for section SEC of EXEC and read it. LEN is the length
   --  of the section. Loaded bytes will be stored in CONTENT, and the mapped
   --  region it comes from is stored in LS. It is up to the caller to free
   --  it after use. The low bound of CONTENT is 0.

   function Load_Section_Content_And_Address
     (Exec    : Exe_File_Type'Class;
      Sec     : Section_Index;
      Len     : in out Elf_Addr;
      Content : in out Binary_Content;
      LS      : in out Loaded_Section;
      Addr    : in out Address)
     return Boolean;
   pragma Inline (Load_Section_Content_And_Address);
   --  Wrapper around Alloc_And_Load_Section: if CONTENT is not already loaded,
   --  allocate memory for section SEC of EXEC and then do the same processing
   --  as Alloc_And_Load_Section. Return True if CONTENT is loaded in the end,
   --  and False if not (this may happen if SEC is missing in the binary file).

   procedure Load_Symtab (Exec : in out Elf_Exe_File_Type);
   --  Load the symbol table (but not the string table) if not already
   --  loaded.

   function Get_Desc_Set
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return access constant Address_Info_Sets.Set;
   pragma Inline (Get_Desc_Set);
   --  Return the Address_Info_Set of type Kind in Exec containing PC

   procedure Open_Exec_Fd
     (File_Name        : String;
      Actual_File_Name : out String_Access;
      Fd               : out GNAT.OS_Lib.File_Descriptor);
   --  Try to open the File_Name executable. This makes multiple attempts: it
   --  may try to append ".exe" (for instance of Windows) to the file name, or
   --  if not found it may look at the current directory.
   --
   --  If successful, put the corresponding file descriptor in Fd and the
   --  file name used in Actual_File_Name (to be free'd by the caller). If
   --  unsuccessful, raise a Binary_Files.Error exception.

   function Tree_Assumption_Respected
     (Set  : Address_Info_Sets.Set;
      Item : Address_Info_Acc) return Boolean;
   --  Return whether, for all elements E of Set, E and Item are either
   --  disjoint or one contains the other.

   -------------------------
   -- Has_Decision_Mapped --
   -------------------------

   function Has_Decision_Mapped (Exec : Exe_File_Type'Class) return Boolean is
   begin
      return Exec.Has_Decision_Mapped;
   end Has_Decision_Mapped;

   -------------------------
   -- Set_Decision_Mapped --
   -------------------------

   procedure Set_Decision_Mapped (Exec : in out Exe_File_Type'Class) is
   begin
      Exec.Has_Decision_Mapped := True;
   end Set_Decision_Mapped;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Address_Info_Acc) return Boolean is
      pragma Assert (L.Kind = R.Kind);

      function Names_Lt (LN, RN : String_Access) return Boolean;
      --  Compare designated strings, null is higher than any non-null string

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
         when Compilation_Unit_Addresses =>
            return L.DIE_CU < R.DIE_CU;
         when Section_Addresses =>
            return Names_Lt (L.Section_Name, R.Section_Name);

         when Subprogram_Addresses =>
            return Names_Lt (L.Subprogram_Name, R.Subprogram_Name);

         when Inlined_Subprogram_Addresses =>
            return L.Call_Sloc < R.Call_Sloc;

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
         when Compilation_Unit_Addresses =>
            return Range_Img & " compilation unit";

         when Section_Addresses =>
            return Range_Img & " section " & El.Section_Name.all;

         when Subprogram_Addresses =>
            return Range_Img & " subprogram " & El.Subprogram_Name.all;

         when Inlined_Subprogram_Addresses =>
            return Range_Img & " inlined subprogram line "
              & Get_Full_Name (El.Call_Sloc.Source_File) & ':'
              & Sloc_Image
                 (Line   => El.Call_Sloc.L.Line,
                  Column => El.Call_Sloc.L.Column);

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

   ------------------
   -- Open_Exec_Fd --
   ------------------

   procedure Open_Exec_Fd
     (File_Name        : String;
      Actual_File_Name : out String_Access;
      Fd               : out GNAT.OS_Lib.File_Descriptor)
   is
      use GNAT.OS_Lib;
      Path : constant String := Lookup_Exec (File_Name);
   begin
      if Path'Length = 0 then
         raise Binary_Files.Error with File_Name & ": File not found";
      end if;

      Fd := Open_Read (Path, Binary);
      if Fd = Invalid_FD then
         raise Binary_Files.Error with "Could not open " & Path;
      end if;

      Actual_File_Name := new String'(Path);
   end Open_Exec_Fd;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (Filename   : String;
      Text_Start : Pc_Type)
      return Exe_File_Acc
   is
      procedure Merge_Architecture
        (Arch          : Unsigned_16;
         Is_Big_Endian : Boolean);
      --  Set Machine or check it.

      procedure Set_Debug_Section (File : in out Exe_File_Type'Class;
                                   Index : Section_Index;
                                   Name : String);
      --  If NAME is the name of a known dwarf debug section, save index.

      ------------------------
      -- Merge_Architecture --
      ------------------------

      procedure Merge_Architecture
        (Arch          : Unsigned_16;
         Is_Big_Endian : Boolean)
      is
      begin
         if ELF_Machine = 0 then
            ELF_Machine := Arch;
            Machine := Decode_EM (Arch);
            Big_Endian_ELF := Is_Big_Endian;
            Big_Endian_ELF_Initialized := True;
            return;

         elsif ELF_Machine /= Arch then
            --  Mixing different architectures.

            Outputs.Fatal_Error ("unexpected architecture for " & Filename);

         elsif Big_Endian_ELF_Initialized
           and then Big_Endian_ELF /= Is_Big_Endian
         then
            Outputs.Fatal_Error ("unexpected endianness for " & Filename);

         else
            Big_Endian_ELF := Is_Big_Endian;
            Big_Endian_ELF_Initialized := True;
         end if;
      end Merge_Architecture;

      -----------------------
      -- Set_Debug_Section --
      -----------------------

      procedure Set_Debug_Section (File : in out Exe_File_Type'Class;
                                   Index : Section_Index;
                                   Name : String) is
      begin
         if Name = ".debug_abbrev" then
            File.Sec_Debug_Abbrev := Index;

         elsif Name = ".debug_info" then
            File.Sec_Debug_Info := Index;

         elsif Name = ".debug_line" then
            File.Sec_Debug_Line := Index;

         elsif Name = ".debug_line_str" then
            File.Sec_Debug_Line_Str := Index;

         elsif Name = ".debug_str" then
            File.Sec_Debug_Str := Index;

         end if;
      end Set_Debug_Section;

      use GNAT.OS_Lib;
      Fd   : File_Descriptor;
      Name : GNAT.Strings.String_Access;

      Ehdr : Elf_Ehdr;

   --  Start of processing for Open_File

   begin
      Open_Exec_Fd (Filename, Name, Fd);
      Inputs.Log_File_Open (Name.all);

      if Is_ELF_File (Fd) then
         declare
            --  Because of a bug in GNAT (see O602-042), we should not invoke
            --  Create_File from an aggregate, so store its result in a local
            --  variable first.

            E_File   : constant Elf_File := Create_File (Fd, Name);
            Exec_Acc : constant Exe_File_Acc := new Elf_Exe_File_Type'
              (Elf_File => E_File,
               others => <>);
            Exec     : Elf_Exe_File_Type renames
               Elf_Exe_File_Type (Exec_Acc.all);

         begin
            Exec.File := Exec.Elf_File'Unchecked_Access;
            Exec.Exe_Text_Start := Text_Start;
            Ehdr := Get_Ehdr (Exec.Elf_File);
            Exec.Is_Big_Endian := Ehdr.E_Ident (EI_DATA) = ELFDATA2MSB;
            Exec.Exe_Machine := Ehdr.E_Machine;

            Merge_Architecture (Exec.Exe_Machine, Exec.Is_Big_Endian);

            case Get_Ehdr (Exec.Elf_File).E_Type is
               when ET_EXEC =>
                  Exec.Kind := File_Executable;
               when ET_REL =>
                  Exec.Kind := File_Object;
               when others =>
                  Exec.Kind := File_Others;
            end case;

            for I in 0 .. Get_Shdr_Num (Exec.Elf_File) - 1 loop
               declare
                  Name : constant String :=
                    Get_Shdr_Name (Exec.Elf_File, I);
               begin
                  if Name = ".symtab" then
                     Exec.Sec_Symtab := I;

                  elsif Exec.Exe_Machine = EM_PPC
                        and then Name = ".PPC.EMB.apuinfo"
                  then
                     declare
                        Len     : Elf_Addr;
                        Content : Binary_Content;
                        LS      : Loaded_Section;
                     begin
                        Alloc_And_Load_Section
                          (Exec, Section_Index (I), Len, Content, LS);
                        Machine := Disa_Ppc.Extract_APU_Info
                          (Filename, Big_Endian_ELF, Content);
                        Free (LS);
                     end;

                  else
                     Set_Debug_Section (Exec, Section_Index (I), Name);
                  end if;
               end;
            end loop;
            return Exec_Acc;
         end;

      elsif Is_PE_File (Fd) then
         declare
            Exec_Acc : constant Exe_File_Acc := new PE_Exe_File_Type'
               (PE_File => Create_File (Fd, Name),
                others => <>);
            Exec : PE_Exe_File_Type renames PE_Exe_File_Type (Exec_Acc.all);
         begin
            Exec.File := Exec.PE_File'Unchecked_Access;
            Exec.Exe_Text_Start := Text_Start;
            --  Ehdr := Get_Ehdr (Exec.Elf_File);
            Exec.Is_Big_Endian := False;
            case Get_Hdr (Exec.PE_File).F_Machine is
               when Coff.I386magic =>
                  Exec.Exe_Machine := EM_386;
               when Coff.AMD64magic =>
                  Exec.Exe_Machine := EM_X86_64;
               when others =>
                  Outputs.Fatal_Error
                    ("unhandled PE architecture for " & Filename);
            end case;

            Merge_Architecture (Exec.Exe_Machine, Exec.Is_Big_Endian);

            if (Get_Hdr (Exec.PE_File).F_Flags and Coff.F_Exec) /= 0 then
               Exec.Kind := File_Executable;
            else
               Exec.Kind := File_Others;
            end if;

            for I in 0 .. Get_Nbr_Sections (Exec.PE_File) - 1 loop
               declare
                  Name : constant String := Get_Section_Name (Exec.PE_File, I);
               begin
                  Set_Debug_Section (Exec, I, Name);
               end;
            end loop;
            return Exec_Acc;
         end;
      else
         Outputs.Fatal_Error ("unknown binary format for " & Filename);
      end if;
   end Open_File;

   --------------------
   -- Close_Exe_File --
   --------------------

   procedure Close_Exe_File (Exec : in out Exe_File_Type) is
   begin
      if Exec.Debug_Str_Section /= No_Loaded_Section then
         Free (Exec.Debug_Str_Section);
      end if;
      Exec.Debug_Str_Base := Null_Address;
      Exec.Debug_Str_Len := 0;

      if Exec.Lines_Section /= No_Loaded_Section then
         Free (Exec.Lines_Section);
      end if;
      Exec.Lines_Len := 0;

      if Exec.Line_Str_Section /= No_Loaded_Section then
         Free (Exec.Line_Str_Section);
      end if;
      Exec.Line_Str_Base := Null_Address;
      Exec.Line_Str_Len := 0;

      if Exec.Symtab_Section /= No_Loaded_Section then
         Free (Exec.Symtab_Section);
      end if;
      Exec.Nbr_Symbols := 0;

      Exec.Sec_Debug_Abbrev   := No_Section;
      Exec.Sec_Debug_Info     := No_Section;
      Exec.Sec_Debug_Line     := No_Section;
      Exec.Sec_Debug_Line_Str := No_Section;
      Exec.Sec_Debug_Str      := No_Section;

      for I_Ranges of Exec.Insn_Set_Ranges loop
         Free (I_Ranges);
      end loop;
   end Close_Exe_File;

   procedure Close_Exe_File (Exec : in out Elf_Exe_File_Type) is
   begin
      Close_File (Exec.Elf_File);

      Close_Exe_File (Exe_File_Type (Exec));

      Exec.Sec_Symtab         := SHN_UNDEF;
   end Close_Exe_File;

   procedure Close_Exe_File (Exec : in out PE_Exe_File_Type) is
   begin
      Close_File (Exec.PE_File);

      Close_Exe_File (Exe_File_Type (Exec));
   end Close_Exe_File;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (Exec : in out Exe_File_Acc) is
   begin
      Close_Exe_File (Exec.all);

      --  FIXME: free contents

      for J in Exec.Desc_Sets'Range loop
         Exec.Desc_Sets (J).Clear;
      end loop;

      Free (Exec);
   end Close_File;

   ----------
   -- Free --
   ----------

   procedure Free (Info : in out Address_Info_Acc) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Address_Info, Address_Info_Acc);
   begin
      if Info = null then
         return;
      end if;

      case Info.Kind is
         when Compilation_Unit_Addresses =>
            null;

         when Section_Addresses =>
            Free (Info.Section_Name);
            Free (Info.Section_LS);

         when Subprogram_Addresses =>
            Free (Info.Subprogram_Name);

         when Inlined_Subprogram_Addresses =>
            null;

         when Symbol_Addresses =>
            Free (Info.Symbol_Name);

         when Line_Addresses =>
            null;
      end case;

      Deallocate (Info);
   end Free;

   -----------------------
   -- Find_Address_Info --
   -----------------------

   function Find_Address_Info
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Sets.Cursor
   is
      use Address_Info_Sets;
      PC_Addr   : aliased Address_Info (Kind);
      Candidate : Address_Info_Sets.Cursor;
      Addr_Info : Address_Info_Acc;
   begin
      --  Empty range with default values sorts higher than any empty range
      --  with non-default values, and higher than any non-empty range,
      --  starting at PC.

      PC_Addr.First := PC;
      PC_Addr.Last  := PC - 1;

      --  Set.Floor is a good initial guess, but needs to be refined to get the
      --  immediatly enclosing Address_Info.
      --
      --  Given the order on Address_Info, PC_Addr will always be greater than
      --  the Address_Info we are looking for (if it exists), so we go through
      --  Set in reverse starting from Set.Floor until we find the Adress_Info
      --  of interest, or until we can conclude that there is no such
      --  Addres_Info.
      --
      --  We can conclude that PC is contained in an Address_Info or not as
      --  soon as we reach a top level Info. If PC is not contained in that top
      --  level Address_Info, then PC isn't contained in any of those.

      Candidate := Set.Floor (PC_Addr'Unchecked_Access);

      loop
         if not Has_Element (Candidate) then
            return No_Element;
         end if;

         Addr_Info := Element (Candidate);
         if PC in Addr_Info.First .. Addr_Info.Last
           or else PC = Addr_Info.First
         then
            return Candidate;
         end if;

         if Addr_Info.Top_Level then
            return No_Element;
         end if;

         Candidate := Previous (Candidate);
      end loop;

   end Find_Address_Info;

   -----------------------
   -- Find_Address_Info --
   -----------------------

   function Find_Address_Info
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Sets.Cursor
   is
      Set : Address_Info_Sets.Set renames Get_Desc_Set (Exec, Kind, PC).all;
   begin
      return Find_Address_Info (Set, Kind, PC);
   end Find_Address_Info;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Exec : Exe_File_Type) return String is
   begin
      return Filename (Exec.File.all);
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
      return Get_Size (Exec.File.all);
   end Get_Size;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp (Exec : Exe_File_Type) return GNAT.OS_Lib.OS_Time is
   begin
      return Get_Time_Stamp (Exec.File.all);
   end Get_Time_Stamp;

   ---------------
   -- Get_CRC32 --
   ---------------

   function Get_CRC32 (Exec : Exe_File_Type) return Unsigned_32 is
   begin
      return Get_CRC32 (Exec.File.all);
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

   --------------------
   -- Get_Strtab_Idx --
   --------------------

   function Get_Strtab_Idx (Exec : Elf_Exe_File_Type) return Elf_Half is
      Symtab_Shdr : Elf_Shdr_Acc;
   begin
      if Exec.Sec_Symtab = SHN_UNDEF then
         return SHN_UNDEF;
      end if;

      Symtab_Shdr := Get_Shdr (Exec.Elf_File, Exec.Sec_Symtab);

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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
     (Exec : Exe_File_Type'Class;
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
         Read_Word4 (Exec, Base, Off, Unsigned_32 (Res));

      elsif Sz = 8 then
         Read_Word8 (Exec, Base, Off, Unsigned_64 (Res));

      else
         raise Program_Error with "unhandled address length";
      end if;
   end Read_Address;

   -------------------------
   -- Read_Dwarf_Form_U64 --
   -------------------------

   procedure Read_Dwarf_Form_U64
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_64)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_addr =>
            Read_Address (Exec, Base, Off, Exec.Addr_Size, Pc_Type (Res));

         when DW_FORM_addrx1
            | DW_FORM_data1
            | DW_FORM_flag
            | DW_FORM_ref1
            | DW_FORM_strx1 =>
            Read_Byte (Base, Off, Unsigned_8 (Res));

         when DW_FORM_addrx2
            | DW_FORM_data2
            | DW_FORM_ref2
            | DW_FORM_strx2 =>
            Read_Word2 (Exec, Base, Off, Unsigned_16 (Res));

         when DW_FORM_addrx4
            | DW_FORM_data4
            | DW_FORM_ref4
            | DW_FORM_ref_sup4
            | DW_FORM_strx4 =>
            Read_Word4 (Exec, Base, Off, Unsigned_32 (Res));

         when DW_FORM_data8
            | DW_FORM_ref8
            | DW_FORM_ref_sup8
            | DW_FORM_ref_sig8 =>
            Read_Word8 (Exec, Base, Off, Res);

         when DW_FORM_sdata =>
            Read_SLEB128 (Base, Off, Unsigned_32 (Res));

         when DW_FORM_addrx
            | DW_FORM_loclistx
            | DW_FORM_ref_udata
            | DW_FORM_rnglistx
            | DW_FORM_strx
            | DW_FORM_udata =>
            Read_ULEB128 (Base, Off, Unsigned_32 (Res));

         when DW_FORM_ref_addr
            | DW_FORM_sec_offset
            | DW_FORM_strp
            | DW_FORM_line_strp
            | DW_FORM_strp_sup =>
            --  This must be changed to 8 for 64-bit DWARF

            Read_Word4 (Exec, Base, Off, Unsigned_32 (Res));

         when DW_FORM_data16
            | DW_FORM_block1
            | DW_FORM_block2
            | DW_FORM_block4
            | DW_FORM_block
            | DW_FORM_exprloc
            | DW_FORM_string =>
            raise Program_Error with "DWARF form too large";

         when DW_FORM_indirect =>
            raise Program_Error with "DW_FORM_indirect unhandled";

         when others =>
            raise Program_Error with
              "unhandled DWARF 64-bit form #" & Unsigned_32'Image (Form);
      end case;
   end Read_Dwarf_Form_U64;

   -------------------------
   -- Read_Dwarf_Form_U32 --
   -------------------------

   procedure Read_Dwarf_Form_U32
     (Exec : Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Unsigned_32)
   is
   begin
      Read_Dwarf_Form_U64 (Exec, Base, Off, Form, Unsigned_64 (Res));
   end Read_Dwarf_Form_U32;

   ----------------------------
   -- Read_Dwarf_Form_String --
   ----------------------------

   procedure Read_Dwarf_Form_String
     (Exec : in out Exe_File_Type'Class;
      Base : Address;
      Off  : in out Storage_Offset;
      Form : Unsigned_32;
      Res  : out Address)
   is
      use Dwarf;
   begin
      case Form is
         when DW_FORM_line_strp =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Exec, Base, Off, V);
               if Load_Section_Content_And_Address
                    (Exec,
                     Exec.Sec_Debug_Line_Str,
                     Exec.Line_Str_Len,
                     Exec.Line_Str,
                     Exec.Line_Str_Section,
                     Exec.Line_Str_Base)
               then
                  Res := Exec.Line_Str_Base + Storage_Offset (V);
               else
                  Res := Null_Address;
               end if;
            end;

         when DW_FORM_strp =>
            declare
               V : Unsigned_32;
            begin
               Read_Word4 (Exec, Base, Off, V);
               if Load_Section_Content_And_Address
                    (Exec,
                     Exec.Sec_Debug_Str,
                     Exec.Debug_Str_Len,
                     Exec.Debug_Str,
                     Exec.Debug_Str_Section,
                     Exec.Debug_Str_Base)
               then
                  Res := Exec.Debug_Str_Base + Storage_Offset (V);
               else
                  Res := Null_Address;
               end if;
            end;

         when DW_FORM_string =>
            Res := Base + Off;
            Read_String (Base, Off);

         when others =>
            raise Program_Error with
              "unhandled DWARF string form #" & Unsigned_32'Image (Form);
      end case;
   end Read_Dwarf_Form_String;

   ---------------------
   -- Skip_Dwarf_Form --
   ---------------------

   procedure Skip_Dwarf_Form
     (Exec : Exe_File_Type'Class;
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

         when DW_FORM_block | DW_FORM_exprloc =>
            declare
               V : Unsigned_32;
            begin
               Read_ULEB128 (Base, Off, V);
               Off := Off + Storage_Offset (V);
            end;

         when DW_FORM_addrx1
            | DW_FORM_data1
            | DW_FORM_flag
            | DW_FORM_ref1
            | DW_FORM_strx1 =>
            Off := Off + 1;

         when DW_FORM_addrx2
            | DW_FORM_data2
            | DW_FORM_ref2
            | DW_FORM_strx2 =>
            Off := Off + 2;

         when DW_FORM_addrx3
            | DW_FORM_strx3 =>
            Off := Off + 3;

         when DW_FORM_addrx4
            | DW_FORM_data4
            | DW_FORM_ref4
            | DW_FORM_ref_sup4
            | DW_FORM_strx4 =>
            Off := Off + 4;

         when DW_FORM_data8
            | DW_FORM_ref8
            | DW_FORM_ref_sup8
            | DW_FORM_ref_sig8 =>
            Off := Off + 8;

         when DW_FORM_data16 =>
            Off := Off + 16;

         when DW_FORM_sdata =>
            declare
               V : Unsigned_32;
            begin
               Read_SLEB128 (Base, Off, V);
            end;

         when DW_FORM_addrx
            | DW_FORM_loclistx
            | DW_FORM_ref_udata
            | DW_FORM_rnglistx
            | DW_FORM_strx
            | DW_FORM_udata =>
            declare
               V : Unsigned_32;
            begin
               Read_ULEB128 (Base, Off, V);
            end;

         when DW_FORM_flag_present | DW_FORM_implicit_const =>
            --  This flag is implicitely present, so it is not materialized
            --  outside abbreviations.

            null;

         when DW_FORM_ref_addr
            | DW_FORM_sec_offset
            | DW_FORM_strp
            | DW_FORM_line_strp
            | DW_FORM_strp_sup =>
            --  This must be changed to 8 for 64-bit DWARF

            Off := Off + 4;

         when DW_FORM_string =>
            Read_String (Base, Off);

         when DW_FORM_indirect =>
            raise Program_Error with "DW_FORM_indirect unhandled";

         when others =>
            raise Program_Error with
              "unhandled DWARF form #" & Unsigned_32'Image (Form);
      end case;
   end Skip_Dwarf_Form;

   -----------------------
   -- Apply_Relocations --
   -----------------------

   procedure Apply_Relocations
     (Exec    : in out Elf_Exe_File_Type;
      Sec_Idx : Section_Index;
      LS      : in out Loaded_Section;
      Data    : in out Binary_Content)
   is
      Sec_Rel        : Elf_Half;
      Relocs_Len     : Elf_Addr;
      Relocs         : Binary_Content;
      Relocs_Section : Loaded_Section;

      Sym_Num : Unsigned_32;
      Sym     : Elf_Sym;

      Shdr : Elf_Shdr_Acc;
      Off  : Storage_Offset;

      Offset : Elf_Addr;
      R      : Elf_Rela;

   begin
      --  The only sections to which we have to apply relocations are the
      --  .debug_info and the .debug_line sections. These sections have
      --  relocations in object files only, i.e. before linking. In this
      --  case, sections do not have their address assigned yet, and thus
      --  we can consider that they are located at 0x0.

      --  We noticed that in this case (relocations for debug sections in
      --  object files), the symbols targeted by the relocations are
      --  sections themselves, and they do not have any addend (.rel.*
      --  relocation sections).

      --  So in this particular configuration, there is no need to relocate
      --  debug sections since it would only add section addresses (= 0x0) to
      --  the offsets already present in the debug sections. Thus, we do not
      --  handle relocation sections without addend.

      --  Find relocation section

      Sec_Rel := 0;
      for I in 0 .. Get_Nbr_Sections (Exec.Elf_File) - 1 loop
         Shdr := Get_Shdr (Exec.Elf_File, Elf_Half (I));
         if Shdr.Sh_Type = SHT_RELA
           and then Shdr.Sh_Info = Elf_Word (Sec_Idx)
         then
            Sec_Rel := Elf_Half (I);
            exit;
         end if;
      end loop;

      if Sec_Rel = 0 then
         return;
      end if;
      if Shdr.Sh_Type /= SHT_RELA then
         raise Program_Error;
      end if;
      if Natural (Shdr.Sh_Entsize) /= Elf_Rela_Size then
         raise Program_Error;
      end if;
      if Shdr.Sh_Size mod Pc_Type (Elf_Rela_Size) /= 0 then
         raise Program_Error;
      end if;

      Make_Mutable (Exec.Elf_File, LS);

      Alloc_And_Load_Section
        (Exec, Section_Index (Sec_Rel), Relocs_Len, Relocs, Relocs_Section);
      if Relocs_Len /= Shdr.Sh_Size then
         raise Program_Error;
      end if;

      Load_Symtab (Exec);

      Off := 0;
      while Off < Storage_Offset (Relocs_Len) loop
         --  Read relocation entry

         R := Get_Rela (Exec.Elf_File, Address_Of (Relocs, Elf_Addr (Off)));
         Off := Off + Storage_Offset (Elf_Rela_Size);

         if R.R_Offset > Data.Last then
            raise Program_Error with "relocation offset beyond section size";
         end if;

         Sym_Num := Elf_R_Sym (R.R_Info);
         if Sym_Num > Unsigned_32 (Exec.Nbr_Symbols) then
            raise Program_Error with "invalid symbol number in relocation";
         end if;
         Sym := Get_Sym
           (Exec.Elf_File,
            Address_Of
              (Exec.Symtab, Elf_Addr (Sym_Num) * Elf_Addr (Elf_Sym_Size)));

         if Elf_St_Type (Sym.St_Info) = STT_SECTION then
            Offset := Get_Shdr (Exec.Elf_File,
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
            when EM_LMP =>
               case Elf_R_Type (R.R_Info) is
                  when R_LMP_32 =>
                     Write_Word4 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_32 (Offset
                                    + Elf_Addr (R.R_Addend)));
                  when others =>
                     raise Program_Error with "unhandled LMP relocation";
               end case;
            when EM_AARCH64 =>
               case Elf_R_Type (R.R_Info) is
                  when R_AARCH64_ABS32 =>
                     Write_Word4 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_32 (Offset
                                    + Elf_Addr (R.R_Addend)));
                  when R_AARCH64_ABS64 =>
                     Write_Word8 (Exec,
                                  Address_Of (Data, 0),
                                  Storage_Offset (R.R_Offset),
                                  Unsigned_64 (Offset
                                    + Elf_Addr (R.R_Addend)));
                  when others =>
                     raise Program_Error with "unhandled AARCH64 relocation" &
                     Elf_Word'Image (Elf_R_Type (R.R_Info));
               end case;
            when others =>
               Outputs.Fatal_Error
                 ("Relocs unhandled for this machine, reloc is"
                  & Elf_Word'Image (Elf_R_Type (R.R_Info)));
         end case;

      end loop;
      Free (Relocs_Section);
   end Apply_Relocations;

   procedure Apply_Relocations
     (Exec    : in out PE_Exe_File_Type;
      Sec_Idx : Section_Index;
      LS      : in out Loaded_Section;
      Data    : in out Binary_Content) is
   begin
      --  Not handled for PE-COFF

      null;
   end Apply_Relocations;

   ----------------------------
   -- Alloc_And_Load_Section --
   ----------------------------

   procedure Alloc_And_Load_Section
     (Exec    : Exe_File_Type'Class;
      Sec     : Section_Index;
      Len     : out Elf_Addr;
      Content : out Binary_Content;
      LS      : out Loaded_Section)
   is
   begin
      if Sec /= No_Section then
         Len := Get_Section_Length (Exec.File.all, Sec);
         pragma Assert (Len > 0);

         LS := Load_Section (Exec.File.all, Sec);
         Content := Binary_Files.Content (LS);
      else
         Len := 0;
         LS := No_Loaded_Section;
         Content := Invalid_Binary_Content;
      end if;
   end Alloc_And_Load_Section;

   -------------------------------
   -- Build_Debug_Compile_Units --
   -------------------------------

   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type'Class) is
      use Dwarf;
      use Compile_Unit_Vectors;

      function Symbol_Exists (Low : Pc_Type; Name : Address) return Boolean;
      --  Return whether there exists a symbol at Low whose name is Name.
      --  Return False if Name is Null_Address.

      -------------------
      -- Symbol_Exists --
      -------------------

      function Symbol_Exists (Low : Pc_Type; Name : Address) return Boolean is
         Sym : Address_Info_Acc;
      begin
         if Name = Null_Address then
            return False;
         end if;

         Sym := Get_Address_Info (Exec, Symbol_Addresses, Low);
         if Sym = null then
            return False;
         end if;

         declare
            Name_Str : constant String := Read_String (Name);
         begin
            return Name_Str = Sym.Symbol_Name.all;
         end;
      end Symbol_Exists;

      Abbrev_Len      : Elf_Addr;
      Abbrevs         : Binary_Content;
      Abbrevs_Section : Loaded_Section;
      Abbrev_Base     : Address;
      Map             : Abbrev_Map_Acc;
      Abbrev          : Address;

      Info_Len              : Elf_Addr;
      Infos                 : Binary_Content;
      Infos_Section         : Loaded_Section;
      Base                  : Address;
      Off, Sec_Off, Tag_Off : Storage_Offset;
      Aoff                  : Storage_Offset;

      Len        : Unsigned_32;
      Version    : Unsigned_16;
      Abbrev_Off : Unsigned_32;
      Unit_Type  : Unsigned_8;
      Ptr_Sz     : Unsigned_8;
      Last       : Storage_Offset;
      Num        : Unsigned_32;

      Tag  : Unsigned_32;
      Name : Unsigned_32;
      Form : Unsigned_32;
      Cst  : Unsigned_32;

      At_Stmt_List       : Unsigned_32;
      At_Low_Pc          : Unsigned_64;
      At_High_Pc         : Unsigned_64;
      At_Lang            : Unsigned_64;
      At_Call_File       : Unsigned_32;
      At_Call_Line       : Unsigned_32;
      At_Call_Column     : Unsigned_32;
      At_Name            : Address;
      At_Comp_Dir        : Address;
      At_Linkage_Name    : Address;
      At_Abstract_Origin : Unsigned_64;

      Current_Sec       : Address_Info_Acc;
      Current_Subprg    : Address_Info_Acc;
      Current_CU        : CU_Id := No_CU_Id;
      Current_DIE_CU    : DIE_CU_Id := No_DIE_CU_Id;
      Current_Stmt_List : Unsigned_32 := No_Stmt_List;
      Compilation_Dir   : String_Access;
      Unit_Filename     : String_Access;
      Subprg_Low        : Pc_Type;

      Is_High_Pc_Offset : Boolean := False;
      --  The DWARF standard defines two ways to interpret DW_AT_high_pc
      --  attributes: it can be encoded as an address or as a constant,
      --  in which case it represents the offset from the low PC.

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

      Subprg_To_PC        : Subprg_DIE_To_PC_Maps.Map;
      Call_Site_To_Target : Call_Site_To_Target_Maps.Vector;

   --  Start of processing for Build_Debug_Compile_Units

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
                              Abbrev_Len, Abbrevs, Abbrevs_Section);
      Abbrev_Base := Address_Of (Abbrevs, 0);

      Map := null;

      --  Load .debug_info

      Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Info,
                              Info_Len, Infos, Infos_Section);
      Base := Address_Of (Infos, 0);

      --  Load symbols

      Build_Symbols (Exec);

      Apply_Relocations (Exec, Exec.Sec_Debug_Info, Infos_Section, Infos);

      Off := 0;
      CU_Loop : while Off < Storage_Offset (Info_Len) loop
         --  Read .debug_info header:

         Sec_Off := Off;
         Read_Word4 (Exec, Base, Off, Len);
         if Len >= 16#ffff_fff0# then

            --  It looks like GCC never produces such sections, so they are not
            --  supported at the moment.

            raise Program_Error with "Unsupported 64-bit DWARF section";
         end if;

         Last := Off + Storage_Offset (Len);
         Read_Word2 (Exec, Base, Off, Version);

         if Version >= 5 then
            Read_Byte (Base, Off, Unit_Type);
            Read_Byte (Base, Off, Ptr_Sz);
            Read_Word4 (Exec, Base, Off, Abbrev_Off);

         elsif Version >= 2 then
            Read_Word4 (Exec, Base, Off, Abbrev_Off);
            Read_Byte (Base, Off, Ptr_Sz);

         else
            Outputs.Fatal_Error ("Unsupported DWARF version:" & Version'Img);
         end if;

         Exec.Addr_Size := Natural (Ptr_Sz);

         Build_Abbrev_Map (Abbrev_Base + Storage_Offset (Abbrev_Off), Map);

         --  Read DIEs

         DIE_Loop : loop
         <<Again>>
            exit DIE_Loop when Off >= Last;
            Tag_Off := Off;
            Read_ULEB128 (Base, Off, Num);
            if Num = 0 then
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

            --  Skip child flag

            Aoff := Aoff + 1;

            --  Read attributes

            At_Stmt_List := No_Stmt_List;
            At_Low_Pc := 0;
            At_High_Pc := 0;
            At_Lang := 0;
            At_Call_File := 0;
            At_Call_Line := 0;
            At_Call_Column := 0;
            At_Name := Null_Address;
            At_Comp_Dir := Null_Address;
            At_Linkage_Name := Null_Address;
            At_Abstract_Origin := 0;

            Is_High_Pc_Offset := False;

            Attr_Loop : loop
               Read_ULEB128 (Abbrev, Aoff, Name);
               Read_ULEB128 (Abbrev, Aoff, Form);

               --  DW_FORM_implicit_const takes its value from the table

               if Form = DW_FORM_implicit_const then
                  Read_SLEB128 (Abbrev, Aoff, Cst);
               end if;

               exit Attr_Loop when Name = 0 and then Form = 0;

               case Name is
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
                  when DW_AT_low_pc =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_Low_Pc);
                  when DW_AT_high_pc =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_High_Pc);
                     Is_High_Pc_Offset := Form /= DW_FORM_addr;
                  when DW_AT_language =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form, At_Lang);
                  when DW_AT_abstract_origin =>
                     Read_Dwarf_Form_U64 (Exec, Base, Off, Form,
                                          At_Abstract_Origin);

                     --  References to other DIEs are relative to the beginning
                     --  of the current compile unit.

                     At_Abstract_Origin :=
                        Unsigned_64 (Sec_Off) + At_Abstract_Origin;
                  when DW_AT_call_file =>
                     if Form = DW_FORM_implicit_const then
                        At_Call_File := Cst;
                     else
                        Read_Dwarf_Form_U32 (Exec, Base, Off, Form,
                                             At_Call_File);
                     end if;
                  when DW_AT_call_line =>
                     if Form = DW_FORM_implicit_const then
                        At_Call_Line := Cst;
                     else
                        Read_Dwarf_Form_U32 (Exec, Base, Off, Form,
                                             At_Call_Line);
                     end if;
                  when DW_AT_call_column =>
                     if Form = DW_FORM_implicit_const then
                        At_Call_Column := Cst;
                     else
                        Read_Dwarf_Form_U32 (Exec, Base, Off, Form,
                                             At_Call_Column);
                     end if;
                  when others =>
                     Skip_Dwarf_Form (Exec, Base, Off, Form);
               end case;
            end loop Attr_Loop;

            --  Debug info for ARM subprograms may use low addresses with the
            --  LSB set to indicate that they target Thumb code (all code is
            --  aligned at least on 2 bytes, so this does not create
            --  ambiguity). So the address that is read in DWARF actually
            --  contains two separate information: the actual low PC for this
            --  DIE, and whether it's ARM or Thumb code.  Our data structures
            --  are designed to contain the low PC only, and ARM ELF binaries
            --  contain $a/$d/$t symbols to recover the ARM/Thumb information
            --  so we can afford to take it out here.

            if Machine = ARM and then At_Low_Pc mod 2 = 1 then
               At_Low_Pc := At_Low_Pc - 1;
            end if;

            --  Patch the high PC only now, since it may need the value for the
            --  DW_AT_low_pc attribute and we cannot assume that DW_AT_low_pc
            --  appears before DW_AT_high_pc.

            if Is_High_Pc_Offset then
               At_High_Pc := At_Low_Pc + At_High_Pc;
            end if;

            case Tag is
               when DW_TAG_compile_unit =>

                  --  If we know that this compilation unit is for a language
                  --  we do not support, just skip it. This is an optimization,
                  --  but also removes the need for us from having to support
                  --  foreign constructs.
                  --
                  --  As we want to support object coverage for possibly other
                  --  languages than Ada / C, we will keep analyzing the
                  --  compilation unit in that case.

                  if not Object_Coverage_Enabled
                     and then At_Lang not in DW_LANG_C
                                           | DW_LANG_C89
                                           | DW_LANG_C99
                                           | DW_LANG_C11
                                           | DW_LANG_Ada83
                                           | DW_LANG_Ada95
                  then
                     Off := Last;
                     exit DIE_Loop;
                  end if;

                  if At_Comp_Dir /= Null_Address then
                     Compilation_Dir := new String'(Read_String (At_Comp_Dir));
                  else
                     Compilation_Dir := null;
                  end if;

                  Unit_Filename := new String'(Read_String (At_Name));

                  --  If we have an entry for the unit in the files table at
                  --  this point, we know it is for an unit of interest and it
                  --  might be that only the file simple name is registered.
                  --  Update the tables with the possibly full path name we
                  --  have at hand from DW_AT_name.
                  --
                  --  Don't touch the tables otherwise. The path we have might
                  --  then be for an unit not of interest and trigger
                  --  consolidation conflicts later on.

                  declare
                     Unit_File : Source_File_Index :=
                        Get_Index_From_Generic_Name
                          (Name   => Unit_Filename.all,
                           Kind   => Source_File,
                           Insert => False);
                  begin

                     --  Do not load info for files that are always ignored

                     if Unit_File /= No_Source_File
                       and then Get_File (Unit_File).Ignore_Status /= Always
                     then
                        Unit_File := Get_Index_From_Generic_Name
                          (Name   => Unit_Filename.all,
                           Kind   => Source_File,
                           Insert => True);
                        Current_CU := Comp_Unit (Unit_File);
                     else
                        Current_CU := No_CU_Id;
                     end if;
                  end;

                  --  Mark unit as having code in the executable, to silence
                  --  warning about unit of interest not present in test cases.
                  --  Note: there might be no sloc referring to any source
                  --  file of this unit, for example if it is a library level
                  --  generic instance.

                  if Current_CU /= No_CU_Id then
                     Set_Unit_Has_Code (Current_CU);
                  end if;

                  Exec.Compile_Units.Append
                    (Compile_Unit_Desc'(Unit_Filename,
                                        Compilation_Dir,
                                        At_Stmt_List,
                                        Pc_Type (At_Low_Pc),
                                        Pc_Type (At_High_Pc)));
                  Current_DIE_CU := DIE_CU_Id (Exec.Compile_Units.Length);

                  if At_High_Pc > At_Low_Pc then

                     declare
                        Item : Address_Info_Acc :=
                           new Address_Info'
                             (Kind      => Compilation_Unit_Addresses,
                              First     => Exec.Exe_Text_Start
                              + Pc_Type (At_Low_Pc),
                              Last      => Pc_Type (At_High_Pc - 1),
                              Top_Level => <>,
                              Parent    => null,
                              DIE_CU    => Current_DIE_CU);
                     begin
                        Insert_With_Top_Level_Update
                          (Exec.Desc_Sets (Compilation_Unit_Addresses), Item);
                     end;
                  end if;

                  Current_Stmt_List := At_Stmt_List;

               when DW_TAG_subprogram =>
                  if At_High_Pc > At_Low_Pc then
                     --  It looks like this subprogram is present in this
                     --  compile unit.

                     Subprg_Low := Exec.Exe_Text_Start + Pc_Type (At_Low_Pc);
                     if Current_Sec = null
                       or else
                       Subprg_Low not in Current_Sec.First .. Current_Sec.Last
                     then
                        Current_Sec := Get_Address_Info
                          (Exec, Section_Addresses, Subprg_Low);
                     end if;

                     if Current_Sec = null then
                        --  When the linker eliminate a subprogram (this can
                        --  happen when compiling with -ffunction-sections),
                        --  the debug info for it may remain unrelocated in
                        --  the output ELF.

                        --  In DWARFv4, DW_AT_high_pc may contain an offset, so
                        --  it may be greater than DW_AT_low_pc (which is then
                        --  0) even in this case. That's why we must handle
                        --  this particular case here: these subprograms have
                        --  no matching code section.

                        if At_Low_Pc /= Unsigned_64 (No_PC) then
                           raise Program_Error
                             with "no section for subprogram";
                        end if;

                     --  On ARM, we have seen runtime code at 0x0 so the above
                     --  guard isn't useful there for the -ffunction-sections
                     --  case. For this special case, use another heuristic
                     --  to discard eliminated programs: see if we can find a
                     --  symbol with the same name. If there is no such symbol,
                     --  then the subprogram was very likely eliminated.

                     elsif At_Low_Pc /= Unsigned_64 (No_PC)
                       or else
                         Symbol_Exists (Subprg_Low,
                                        (if At_Linkage_Name = Null_Address
                                         then At_Name
                                         else At_Linkage_Name))
                     then
                        Current_Subprg :=
                          new Address_Info'
                            (Kind              => Subprogram_Addresses,
                             First             => Subprg_Low,
                             Last              =>
                               Exec.Exe_Text_Start + Pc_Type (At_High_Pc - 1),
                             Parent            => Current_Sec,
                             Top_Level         => <>,
                             Subprogram_Name   =>
                                new String'(Read_String (At_Name)),
                             Subprogram_CU     => Current_CU,
                             Subprogram_DIE_CU => Current_DIE_CU,
                             Lines             => Address_Info_Sets.Empty_Set);
                        Insert_With_Top_Level_Update
                          (Exec.Desc_Sets (Subprogram_Addresses),
                           Current_Subprg);

                        Subprg_To_PC.Insert (Tag_Off, Pc_Type (At_Low_Pc));
                     end if;

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

               when DW_TAG_GNU_call_site | DW_TAG_call_site =>
                  if At_Low_Pc /= 0 and then At_Abstract_Origin /= 0 then
                     Call_Site_To_Target.Append
                       (Call_Target'
                          (Pc_Type (At_Low_Pc),
                           Storage_Offset (At_Abstract_Origin)));
                  end if;

               when DW_TAG_inlined_subroutine =>
                  if At_High_Pc > At_Low_Pc then
                     --  TODO: handle address ranges

                     Exec.Inlined_Subprograms.Append
                       (Inlined_Subprogram_Raw'
                          (First            => Pc_Type (At_Low_Pc),
                           Last             => Pc_Type (At_High_Pc) - 1,
                           Stmt_List_Offset => Current_Stmt_List,
                           Section          => Current_Sec,
                           File             => Natural (At_Call_File),
                           Line             => Natural (At_Call_Line),
                           Column           => Natural (At_Call_Column)));
                  end if;

               when others =>
                  null;
            end case;

         end loop DIE_Loop;
         Free (Map);
      end loop CU_Loop;

      --  If there is no debug information in this binary, the following
      --  sections may not have been loaded.

      if Infos_Section /= No_Loaded_Section then
         Free (Infos_Section);
      end if;
      if Abbrevs_Section /= No_Loaded_Section then
         Free (Abbrevs_Section);
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

   procedure Load_Symtab (Exec : in out Elf_Exe_File_Type) is
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

      Alloc_And_Load_Section (Exec, Section_Index (Exec.Sec_Symtab),
                              Symtab_Len, Exec.Symtab, Exec.Symtab_Section);
      Symtab_Shdr := Get_Shdr (Exec.Elf_File, Exec.Sec_Symtab);
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

   --  Up to DWARF 4, the current directory and the current compilation file,
   --  both implicitly referred to by index 0, are not represented resp. in
   --  the Directories and File_Names fields of the header. But, in DWARF 5,
   --  they are explicitly represented resp. in them with index 0.

   package Filenames_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => String_Access);

   package File_Indices_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Source_File_Index);

   ----------------------
   -- Read_Debug_Lines --
   ----------------------

   procedure Read_Debug_Lines
     (Exec                  : in out Exe_File_Type'Class;
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

      Total_Len       : Unsigned_32;
      Version         : Unsigned_16;
      Header_Len      : Unsigned_32;
      Address_Size    : Unsigned_8;
      Seg_Sel_Size    : Unsigned_8;
      Min_Insn_Len    : Unsigned_8;
      Max_Op_Per_Insn : Unsigned_8;
      Dflt_Is_Stmt    : Unsigned_8;
      Line_Base       : Unsigned_8;
      Line_Range      : Unsigned_8;
      Opc_Base        : Unsigned_8;

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

      type Entry_Format_Pair is record
         C_Type : Unsigned_32;
         Form   : Unsigned_32;
      end record;

      type Entry_Format_Array is array (1 .. 5) of Entry_Format_Pair;

      procedure Read_Entry_Format_Array
        (Base : Address;
         Off  : in out Storage_Offset;
         Res  : out Entry_Format_Array;
         Len  : Unsigned_8);
      --  Read an entry format array, as specified by 6.2.4.1

      Entry_Format_Count : Unsigned_8;
      Entry_Format       : Entry_Format_Array;
      Nbr_Dirnames       : Unsigned_32;
      Nbr_Filenames      : Unsigned_32;
      Dirnames           : Filenames_Vectors.Vector;
      Filenames          : Filenames_Vectors.Vector;
      Str_Address        : Address;
      Str_Len            : Storage_Offset;

      File_Indices : File_Indices_Vectors.Vector;
      --  Cached file indices for Filenames. The index is set to No_Source_File
      --  for source files that are not considered for coverage and thus do not
      --  require debug line information.

      No_File_Of_Interest : Boolean := True;
      --  True when every element of File_Indices is No_Source_File

      procedure Compute_File_Index (Filename : String);
      --  Compute the element in File_Indices for Filename

      Cur_Subprg,
      Cur_Sec    : Address_Info_Sets.Cursor;
      Subprg,
      Sec        : Address_Info_Acc;
      --  Current subprogram and section

      Cache            : Insn_Set_Cache := Empty_Cache;
      Ranges_For_Cache : Insn_Set_Ranges_Cst_Acc := null;

      procedure Set_Parents (PC : Pc_Type; Sloc : Source_Location);
      --  Set the current subprogram and section for PC, which is mapped to
      --  Sloc. Create a subprogram and append it to the database if there
      --  is none.

      Last_Line : Address_Info_Acc := null;

      procedure Reset_Lines;
      procedure New_Source_Line;
      procedure Close_Source_Line;
      --  Need comments???

      function Last_For_Insn (First : Pc_Type) return Pc_Type;
      --  Assuming that an instruction starts at First, return the last byte
      --  that is part of that instruction.

      procedure Register_Dirname
        (Dw_Dirname : String;
         Dw_Len     : out Storage_Offset);
      --  From a DWARF directory name provided in Dw_Dirname, full or
      --  relative, compute and add a directory name to our Directories
      --  list. Perform best-effort to come up with a full path name; in
      --  particular, if the input directory name is relative, complete it
      --  with Compilation_Dir if the latter is available. Fill Dw_Len with
      --  the length of Dw_Dirname.

      procedure Register_Filename
        (Dw_Filename : String;
         Dw_Dirindex : Unsigned_32);
      --  For a provided Dwarf filename and file directory index, compute
      --  the filename to add to our filenames table and append it there.
      --  Perform best-effort to come up with a full path name and always
      --  canonicalize.

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

         --  Note: if previous entry is at offset 0 and has an empty range,
         --  this will set its Last to PC_Type'Last, so care must be taken
         --  downstream to not interpret it as covering the whole executable
         --  (see Traces_Elf.Empty_Range/Traces.Empty_Range).

         --  If this entry has a non-empty range, mark it as such using the
         --  Is_Non_Empty flag. If this entry spans over multiple instructions
         --  and there are entries with empty ranges at the same start address,
         --  split this entry in two: one entry to cover the first instruction
         --  (propagate this range to entries with empty ranges) and another to
         --  cover the other instructions.
         --
         --  This is required to handle the semantics of instructions with
         --  several associated slocs, as in:
         --
         --     foo.adb:1:1
         --     foo.adb:2:1
         --     foo.adb:3:1
         --       insn_1
         --       insn_...
         --       insn_n
         --
         --  From the compiler's standpoint, this kind of pattern conveys that
         --  insn_1 pertains to the three slocs while the following
         --  instructions only bind to the last sloc of the sequence.
         --
         --  Later on, two Address_Info entries which start at the same address
         --  are ordered so that the one with the shorter range appears last
         --  (see "<" for Address_Info_Acc), so, overall, we need:
         --
         --    * foo.adb:1:1 and foo.adb:2:1 to appear before foo.adb:3:1,
         --
         --    * foo.adb:1:1 and foo.adb:2:1 to span over only one instruction.
         --
         --  In order to achieve this, we need to split the debug info pattern
         --  above as the following line entries in our internal tables:
         --
         --     foo.adb:1:1
         --     foo.adb:2:1
         --     foo.adb:3:1
         --       insn_1
         --     foo.adb:3:1
         --       insn_...
         --       insn_n

         if not Empty_Range (Last_Line.all) then
            Last_Line.Is_Non_Empty := True;

            declare
               Insn_N_Last_PC : constant Pc_Type := Last_Line.Last;
               --  In the example above, corresponds to the address of the last
               --  byte in the insn_n instruction.

               Other_Lines : constant Address_Info_Arr :=
                  Get_Address_Infos
                    (Subprg.Lines, Line_Addresses, Last_Line.First);
               --  In the example above, all entries which start at the same
               --  address as insn_1.

               Has_Empty_Ranges : Boolean := False;
               --  Whether Other_Lines contains entries with empty ranges

               Insn_1_Last_PC : Pc_Type;
               --  In the example above, corresponds to the address of the last
               --  byte in the insn_1 instruction. Computed only if we find an
               --  empty range entry.

               New_Line : Address_Info_Acc;
            begin
               for Info of Other_Lines loop
                  if Empty_Range (Info.all) then
                     pragma Assert (not Info.Is_Non_Empty);

                     if not Has_Empty_Ranges then
                        Insn_1_Last_PC := Last_For_Insn (Last_Line.First);
                     end if;
                     Has_Empty_Ranges := True;

                     Info.Last := Insn_1_Last_PC;
                  end if;
               end loop;

               --  If the original entry covered multiple instructions and had
               --  associated empty ranged entries, we must split it.

               if Has_Empty_Ranges and then Insn_1_Last_PC /= Insn_N_Last_PC
               then

                  --  Shorten the range of the existing entry to cover only one
                  --  instruction.

                  Last_Line.Last := Insn_1_Last_PC;

                  --  Create a new entry with the same information, except that
                  --  it will cover remaining instructions.

                  New_Line := new Address_Info'(Last_Line.all);
                  New_Line.First := Insn_1_Last_PC + 1;
                  New_Line.Last := Insn_N_Last_PC;
                  New_Line.Is_Non_Empty := True;
                  Insert_With_Top_Level_Update (Subprg.Lines, New_Line);

               end if;
            end;
         end if;

         Last_Line := null;
      end Close_Source_Line;

      ------------------------
      -- Compute_File_Index --
      ------------------------

      procedure Compute_File_Index (Filename : String) is
         Filter_Lines : constant Boolean := Source_Coverage_Enabled;
         Kind         : constant Files_Table.File_Kind :=
                          (if Source_Coverage_Enabled
                           then Stub_File
                           else Source_File);
         --  For source coverage, it's the coverage obligations that
         --  determine which source files are relevant for coverage
         --  analysis. In this case, consider that other source files are
         --  stubs to reduce simple name collisions.

         File_Index : Source_File_Index;

      begin
         --  Optimization: in source coverage, we do not want to add new
         --  files to the files table: the ones added when loading SCOs
         --  are enough. Do not even load debug line information for
         --  files that don't have statement SCOs.

         File_Index := Get_Index_From_Full_Name
                         (Filename, Kind, Insert => not Filter_Lines);

         if Filter_Lines and then File_Index /= No_Source_File then
            declare
               FI : constant File_Info_Access := Get_File (File_Index);

            begin
               if FI.Kind /= Source_File
                 or else not FI.Has_Source_Coverage_Info
               then
                  File_Index := No_Source_File;
               end if;
            end;
         end if;

         if File_Index /= No_Source_File then
            --  This file is of interest. If not already done thanks to the
            --  first call to Get_Index_From_Full_Name, give a chance to the
            --  file table entry to get a full path.

            No_File_Of_Interest := False;
            if Filter_Lines then
               File_Index := Get_Index_From_Full_Name
                               (Filename, Kind, Insert => True);
            end if;
         end if;

         File_Indices.Append (File_Index);
      end Compute_File_Index;

      ---------------------
      -- New_Source_Line --
      ---------------------

      procedure New_Source_Line is
         use Address_Info_Sets;

         Pos        : Cursor;
         Inserted   : Boolean;
         File_Index : Source_File_Index;
         Sloc       : Source_Location;

         Saved_Line : constant Address_Info_Acc := Last_Line;

      begin
         --  Discard 0-relative entries in exec files, corresponding to
         --  regions garbage collected by gc-section.

         if Base_Pc = 0 and then Exec.Kind = File_Executable then
            return;
         end if;

         Close_Source_Line;

         --  Note: Last will be updated by Close_Source_Line

         File_Index := File_Indices.Element (File);
         if File_Index /= No_Source_File then
            Sloc :=
              (Source_File => File_Index,
               L           => (Natural (Line), Natural (Column)));

            Set_Parents (Exec.Exe_Text_Start + Pc, Sloc);

            if Subprg /= null then
               Last_Line :=
                 new Address_Info'
                   (Kind         => Line_Addresses,
                    First        => Exec.Exe_Text_Start + Pc,
                    Last         => Exec.Exe_Text_Start + Pc,
                    Top_Level    => <>,
                    Parent       => (if Subprg /= null then Subprg else Sec),
                    Sloc         => Sloc,
                    Disc         => Disc,
                    Is_Non_Empty => False);
               Insert_With_Top_Level_Update
                 (Subprg.Lines, Last_Line, Pos, Inserted);

               if not Inserted then

                  --  An empty line has already been inserted at PC. Merge it
                  --  with current line.

                  Last_Line := Element (Pos);
               end if;
            end if;

         --  If this entry was ignored, but it was for the same PC as the
         --  last processed entry (case of an instruction bearing multiple
         --  slocs, one of which is ignored), then restore Last_Line.

         elsif Saved_Line /= null
           and then Saved_Line.First = Exec.Exe_Text_Start + Pc
         then
            Last_Line := Saved_Line;
         end if;
         Disc := 0;
      end New_Source_Line;

      -----------------------------
      -- Read_Entry_Format_Array --
      -----------------------------

      procedure Read_Entry_Format_Array
        (Base : Address;
         Off  : in out Storage_Offset;
         Res  : out Entry_Format_Array;
         Len  : Unsigned_8)
      is
         C_Type, Form : Unsigned_32;
         N            : Integer;

      begin
         N := Res'First;

         for J in 1 .. Len loop
            Read_ULEB128 (Base, Off, C_Type);
            Read_ULEB128 (Base, Off, Form);

            case C_Type is
               when DW_LNCT_path .. DW_LNCT_MD5 =>
                  if N not in Res'Range then
                     raise Program_Error with "duplicate DWARF content type";
                  end if;

                  Res (N) := (C_Type, Form);
                  N := N + 1;

               when DW_LNCT_lo_user .. DW_LNCT_hi_user =>
                  null;

               when others =>
                  raise Program_Error with "unhandled DWARF content type";
            end case;
         end loop;
      end Read_Entry_Format_Array;

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

      procedure Set_Parents (PC : Pc_Type; Sloc : Source_Location) is
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

                  --  Stop when we are sure we won't meet a matching
                  --  subprogram.

                  if PC < Cache.First then
                     exit;

                  --  Or stop when me met it

                  elsif PC <= Cache.Last then
                     return;
                  end if;

                  Next (Cur);
               end loop;

               --  If we reach here, we haven't found any matching subprogram

               Cache := null;
            end if;
         end Set_Parent;

      begin
         Set_Parent (Section_Addresses,    Cur_Sec,    Sec);
         Set_Parent (Subprogram_Addresses, Cur_Subprg, Subprg);

         if Subprg = null then
            --  Create a subprogram if there is none for PC

            declare
               use Traces_Names;

               Symbol   : constant Address_Info_Acc :=
                 Get_Address_Info
                   (Exec.Desc_Sets (Symbol_Addresses),
                    Symbol_Addresses,
                    PC);
               Inserted : Boolean;

               SCO      : SCO_Id := No_SCO_Id;
               Complain : Boolean := False;
            begin
               if Symbol = null then
                  --  The code at PC has debug line information, but no
                  --  associated symbol: there must be something wrong.

                  --  Try to avoid complaining when the code is not considered
                  --  for coverage.

                  if Source_Coverage_Enabled then
                     SCO := SC_Obligations.Enclosing_Statement
                       (Sloc_To_SCO (Sloc));
                     Complain := SCO /= No_SCO_Id;

                  else
                     --  If the user provided explicitely the routines of
                     --  interest, then code that has no corresponding
                     --  symbol is not considered for coverage.

                     Complain :=
                       Routines_Of_Interest_Origin /= From_Command_Line;
                  end if;

                  if Complain then
                     Diagnostics.Report
                       (Exec'Unrestricted_Access, PC,
                        "code has debug line information, but no symbol",
                        Kind => Diagnostics.Warning);

                     --  If the code maps to source code targetted by SCOs,
                     --  tag the SCOs as having code so that we will emit a
                     --  coverage violation for it. We have no symbol and no
                     --  subprogram, thus we cannot get a tag for this code.

                     if SCO /= No_SCO_Id then
                        Coverage.Source.Set_Basic_Block_Has_Code
                          (SCO, No_SC_Tag);
                     end if;
                  end if;

               else
                  Subprg := new Address_Info'
                    (Kind              => Subprogram_Addresses,
                     First             => Symbol.First,
                     Last              => Symbol.Last,
                     Parent            => Sec,
                     Top_Level         => <>,
                     Subprogram_Name   =>
                        new String'("<None@" & Symbol.Symbol_Name.all & ">"),
                     Subprogram_CU     => No_CU_Id,
                     Subprogram_DIE_CU => No_DIE_CU_Id,
                     Lines             => Address_Info_Sets.Empty_Set);

                  --  And insert it to the database. This subprogram is create
                  --  because there was none for this PC, so inserting must
                  --  work.

                  Insert_With_Top_Level_Update
                    (Exec.Desc_Sets (Subprogram_Addresses),
                     Subprg,
                     Cur_Subprg,
                     Inserted);
                  pragma Assert (Inserted);

               end if;
            end;
         end if;
      end Set_Parents;

      -------------------
      -- Last_For_Insn --
      -------------------

      function Last_For_Insn (First : Pc_Type) return Pc_Type is
         Code_Section : constant Address_Info_Acc :=
            Exec.Get_Address_Info (Section_Addresses, First);
         Ranges       : constant Insn_Set_Ranges_Cst_Acc :=
            Exec.Get_Insn_Set_Ranges (Code_Section.Section_Sec_Idx);
      begin
         Exec.Load_Section_Content (Code_Section);

         --  Reset the cache if the next instruction range lookup works on a
         --  different set of ranges.

         if Ranges /= Ranges_For_Cache then
            Cache := Empty_Cache;
            Ranges_For_Cache := Ranges;
         end if;

         declare
            Insn_Set : constant Insn_Set_Type :=
               Get_Insn_Set (Ranges.all, Cache, Last_Line.First);
            Code     : constant Binary_Content :=
               Slice (Code_Section.Section_Content,
                      Last_Line.First,
                      Code_Section.Section_Content.Last);

            Insn_Length : constant Positive :=
               Disa_For_Machine (Machine, Insn_Set).Get_Insn_Length (Code);
         begin
            return First + Pc_Type (Insn_Length) - 1;
         end;
      end Last_For_Insn;

      ----------------------
      -- Register_Dirname --
      ----------------------

      procedure Register_Dirname
        (Dw_Dirname : String;
         Dw_Len     : out Storage_Offset)
      is
         Dirname : constant String
           := (if Is_Absolute_Path (Dw_Dirname)
                 or else Compilation_Directory = null
               then Dw_Dirname
               else Build_Filename (Compilation_Directory.all, Dw_Dirname));

      begin
         Dirnames.Append (new String'(Dirname));
         Dw_Len := Dw_Dirname'Length;
      end Register_Dirname;

      -----------------------
      -- Register_Filename --
      -----------------------

      procedure Register_Filename
        (Dw_Filename : String;
         Dw_Dirindex : Unsigned_32)
      is
         --  Use the filename we have if it is absolute to begin with, or if we
         --  don't have a directory to prepend. Otherwise, when we have both a
         --  relative file name and a directory indication, just combine the
         --  two together.

         --  First determine a directory indication we could use if we need
         --  one. Up to DWARF 4, the current directory is not represented in
         --  the directory list and a directory index of 0 implicitly refers
         --  to that directory, as found in the DW_AT_comp_dir attribute of
         --  the compilation unit entry.
         --
         --  In DWARF 5, the current directory is always explicitly present in
         --  the directory list, at index 0, and we might have either relative
         --  or absolute paths for file names in file_names[] entries.

         Dir : constant String_Access :=
           (if Dw_Dirindex = 0 and then Version < 5
            then Compilation_Directory
            else Dirnames.Element (Integer (Dw_Dirindex)));

         Filename : constant String
             := (if Is_Absolute_Path (Dw_Filename) or else Dir = null
                 then Dw_Filename
                 else Build_Filename (Dir.all, Dw_Filename));
      begin
         Filenames.Append (Canonicalize_Filename (Filename));
         Compute_File_Index (Filenames.Last_Element.all);
      end Register_Filename;

   --  Start of processing for Read_Debug_Lines

   begin
      --  Load .debug_line

      if not Is_Loaded (Exec.Lines) then
         Alloc_And_Load_Section (Exec, Exec.Sec_Debug_Line,
                                 Exec.Lines_Len, Exec.Lines,
                                 Exec.Lines_Section);
         Apply_Relocations
           (Exec, Exec.Sec_Debug_Line, Exec.Lines_Section, Exec.Lines);
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

      if Version >= 5 then
         Read_Byte (Base, Off, Address_Size);
         Read_Byte (Base, Off, Seg_Sel_Size);
      end if;

      Read_Word4 (Exec, Base, Off, Header_Len);
      Read_Byte (Base, Off, Min_Insn_Len);

      if Version >= 4 then
         Read_Byte (Base, Off, Max_Op_Per_Insn);
      end if;

      Read_Byte (Base, Off, Dflt_Is_Stmt);
      Read_Byte (Base, Off, Line_Base);
      Read_Byte (Base, Off, Line_Range);
      Read_Byte (Base, Off, Opc_Base);

      --  Initial state registers

      Reset_Lines;

      --  Standard_Opcode_Lengths is an array of Opcode_Base bytes specifying
      --  the number of LEB128 operands for each of the standard opcodes.

      Line_Base2 := Unsigned_32 (Line_Base);
      if (Line_Base and 16#80#) /= 0 then
         Line_Base2 := Line_Base2 or 16#Ff_Ff_Ff_00#;
      end if;
      Opc_Length := new Opc_Length_Type (1 .. Opc_Base - 1);
      for I in 1 .. Opc_Base - 1 loop
         Read_Byte (Base, Off, Opc_Length (I));
      end loop;

      --  The Directories table follows. Up to DWARF 4, this is a list of null
      --  terminated strings terminated by a null byte. In DWARF 5, this is a
      --  sequence of Directories_Count entries which are encoded as described
      --  by the Directory_Entry_Format field.

      Dirnames.Clear;

      if Version <= 4 then
         Dirnames.Append (null);  -- implicit current directory
         Nbr_Dirnames := 1;

         loop
            B := Read_Byte (Base + Off);
            exit when B = 0;

            Register_Dirname
              (Dw_Dirname => Read_String (Base + Off),
               Dw_Len     => Str_Len);

            Off := Off + Str_Len + 1;
            Nbr_Dirnames := Nbr_Dirnames + 1;
         end loop;

         Off := Off + 1;

      else
         Read_Byte (Base, Off, Entry_Format_Count);
         Read_Entry_Format_Array (Base, Off, Entry_Format, Entry_Format_Count);

         Read_ULEB128 (Base, Off, Nbr_Dirnames);

         for J in 0 .. Nbr_Dirnames - 1 loop
            for K in 1 .. Integer (Entry_Format_Count) loop
               if Entry_Format (K).C_Type = DW_LNCT_path then
                  Read_Dwarf_Form_String
                    (Exec, Base, Off, Entry_Format (K).Form, Str_Address);

                  Register_Dirname
                    (Dw_Dirname => Read_String (Str_Address),
                     Dw_Len     => Str_Len);
               else
                  Skip_Dwarf_Form (Exec, Base, Off, Entry_Format (K).Form);
               end if;
            end loop;
         end loop;
      end if;

      --  The File_Names table is next. Up to DWARF 4, this is a list of record
      --  containing a null terminated string for the file name, an unsigned
      --  LEB128 directory index in the Directories table, an unsigned LEB128
      --  modification time, and an unsigned LEB128 for the file length; the
      --  table is terminated by a null byte. In DWARF 5, this is a sequence
      --  of File_Names_Count entries which are encoded as described by the
      --  File_Name_Entry_Format field.

      Filenames.Clear;
      File_Indices.Clear;

      if Version <= 4 then
         Filenames.Append (null);  -- implicit current compilation file
         File_Indices.Append (No_Source_File);
         Nbr_Filenames := 1;

         loop
            B := Read_Byte (Base + Off);
            exit when B = 0;

            declare
               Dw_Filename : constant String := Read_String (Base + Off);

            begin
               Off := Off + Dw_Filename'Length + 1;
               Read_ULEB128 (Base, Off, File_Dir);

               Register_Filename (Dw_Filename, File_Dir);

               Read_ULEB128 (Base, Off, File_Time);
               Read_ULEB128 (Base, Off, File_Len);

               Nbr_Filenames := Nbr_Filenames + 1;
            end;
         end loop;

         Off := Off + 1;

      else
         Read_Byte (Base, Off, Entry_Format_Count);
         Read_Entry_Format_Array (Base, Off, Entry_Format, Entry_Format_Count);

         Read_ULEB128 (Base, Off, Nbr_Filenames);

         for J in 0 .. Nbr_Filenames - 1 loop
            Str_Address := Null_Address;
            File_Dir := 0;

            for K in 1 .. Integer (Entry_Format_Count) loop
               if Entry_Format (K).C_Type = DW_LNCT_path then
                  Read_Dwarf_Form_String
                    (Exec, Base, Off, Entry_Format (K).Form, Str_Address);

               elsif Entry_Format (K).C_Type = DW_LNCT_directory_index then
                  case Entry_Format (K).Form is
                     when DW_FORM_data1 =>
                        Read_Byte (Base, Off, Unsigned_8 (File_Dir));

                     when DW_FORM_data2 =>
                        Read_Word2 (Exec, Base, Off, Unsigned_16 (File_Dir));

                     when DW_FORM_udata =>
                        Read_ULEB128 (Base, Off, File_Dir);

                     when others =>
                        raise Program_Error with
                          "invalid DWARF form for DW_LNCT_directory_index";
                  end case;

               else
                  Skip_Dwarf_Form (Exec, Base, Off, Entry_Format (K).Form);
               end if;
            end loop;

            --  If we not skipping this entry for whatever reason, process
            --  what we have.

            if Str_Address /= Null_Address then
               declare
                  Dw_Filename : constant String := Read_String (Str_Address);
               begin
                  Register_Filename (Dw_Filename, File_Dir);
               end;
            end if;
         end loop;
      end if;

      --  If there is no source file of interest in this debug information,
      --  do nothing.

      if No_File_Of_Interest then
         Free (Opc_Length);
         return;
      end if;

      while Off < Last loop

         --  Read code

         Read_Byte (Base, Off, B);
         Old_Off := Off;

         if B = DW_LNS_extended_op then

            --  Extended opcode

            Read_ULEB128 (Base, Off, Ext_Len);
            Old_Off := Off;
            Read_Byte (Base, Off, Ext_Opc);
            case Ext_Opc is
               when DW_LNE_end_sequence =>
                  Close_Source_Line;
                  Reset_Lines;

               when DW_LNE_set_address =>
                  Read_Address (Exec, Base, Off, Arch.Arch_Addr'Size / 8, Pc);
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

      --  Add corresponding inlined subprogram addresses ranges. Not needed if
      --  computing object coverage.

      if Object_Coverage_Enabled then
         Free (Opc_Length);
         return;
      end if;

      for Inlined_Subp of Exec.Inlined_Subprograms loop
         if Inlined_Subp.Stmt_List_Offset = Stmt_List_Offset then
            declare
               File : constant Source_File_Index :=
                 (if Inlined_Subp.File = 0
                  then No_Source_File
                  else Get_Index_From_Full_Name
                    (Full_Name => Filenames.Element (Inlined_Subp.File).all,
                     Kind      => Stub_File,
                     Insert    => False));
               Info : Address_Info_Acc :=
                  new Address_Info'
                    (Kind      => Inlined_Subprogram_Addresses,
                     First     => Inlined_Subp.First,
                     Last      => Inlined_Subp.Last,
                     Top_Level => <>,
                     Parent    => Inlined_Subp.Section,
                     Call_Sloc =>
                       (Source_File => File,
                        L           => (Line   => Inlined_Subp.Line,
                                        Column => Inlined_Subp.Column)));

               Position : Address_Info_Sets.Cursor;
               Inserted : Boolean;
            begin
               --  Sometimes, the compiler generates two inline subprogram
               --  calls with the exact same address ranges. We don't want to
               --  crash in this case, so just do best effort and ignore such
               --  "duplicates".

               Insert_With_Top_Level_Update
                 (Exec.Desc_Sets (Inlined_Subprogram_Addresses),
                  Info,
                  Position,
                  Inserted);

               if not Inserted then
                  Free (Info);
               end if;
            end;
         end if;
      end loop;

      Free (Opc_Length);
   end Read_Debug_Lines;

   -----------------------
   -- Build_Debug_Lines --
   -----------------------

   procedure Build_Debug_Lines (Exec : in out Exe_File_Type'Class) is
   begin
      --  Return now if already loaded

      if Exec.Lines_Section /= No_Loaded_Section then
         return;
      end if;

      --  Be sure compile units are loaded

      Build_Debug_Compile_Units (Exec);

      --  Read all .debug_line

      for Cu of Exec.Compile_Units loop
         Read_Debug_Lines (Exec, Cu.Stmt_List, Cu.Compilation_Directory);
      end loop;
      Exec.Inlined_Subprograms := Inlined_Subprogram_Vectors.Empty_Vector;
   end Build_Debug_Lines;

   --------------------
   -- Build_Sections --
   --------------------

   procedure Build_Sections (Exec : in out Elf_Exe_File_Type) is
      Shdr     : Elf_Shdr_Acc;
      Addr     : Pc_Type;
      Last     : Pc_Type;
      Offset   : Pc_Type;
      Do_Reloc : Boolean;
   begin
      --  Return now if already built

      if not Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      --  Iterate over all section headers

      Offset := 0;
      Do_Reloc := Get_Ehdr (Exec.Elf_File).E_Type = ET_REL;

      if Do_Reloc then
         Enable_Section_Relocation (Exec.Elf_File);
      end if;

      for Idx in 0 .. Get_Shdr_Num (Exec.Elf_File) - 1 loop
         Shdr := Get_Shdr (Exec.Elf_File, Idx);

         --  Only A+X sections are interesting.

         if (Shdr.Sh_Flags and (SHF_ALLOC or SHF_EXECINSTR))
           = (SHF_ALLOC or SHF_EXECINSTR)
           and then Shdr.Sh_Type = SHT_PROGBITS
           and then Shdr.Sh_Size > 0
         then
            if Do_Reloc then
               --  Relocate the current section, making sure we respect its
               --  alignment constraint.

               Offset := (Offset + Shdr.Sh_Addralign - 1)
                 and not (Shdr.Sh_Addralign - 1);

               Shdr.Sh_Addr := Shdr.Sh_Addr + Offset;

               --  Adjust offset so that the next section is relocated after
               --  the current one.

               Offset := Offset + Shdr.Sh_Size;
            end if;

            Addr := Pc_Type (Shdr.Sh_Addr);
            Last := Pc_Type (Shdr.Sh_Addr + Shdr.Sh_Size - 1);

            declare
               Item : Address_Info_Acc :=
                  new Address_Info'
                    (Kind            => Section_Addresses,
                     First           => Addr,
                     Last            => Last,
                     Top_Level       => <>,
                     Parent          => null,
                     Section_Name    => new String'(
                       Get_Shdr_Name (Exec.Elf_File, Idx)),
                     Section_Sec_Idx => Section_Index (Idx),
                     Section_Content => Invalid_Binary_Content,
                     Section_LS      => No_Loaded_Section);
            begin
               Insert_With_Top_Level_Update
                 (Exec.Desc_Sets (Section_Addresses), Item);
            end;
         end if;
      end loop;
   end Build_Sections;

   procedure Build_Sections (Exec : in out PE_Exe_File_Type) is
      use Coff;
      Scn : Scnhdr;
      Addr : Pc_Type;
      Last : Pc_Type;
      Item : Address_Info_Acc;
   begin
      --  Return now if already built

      if not Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      --  Iterate over all section headers

      for Idx in 0 .. Get_Nbr_Sections (Exec.PE_File) - 1 loop
         Scn := Get_Scnhdr (Exec.PE_File, Idx);

         --  Only TEXT sections are interesting.

         if (Scn.S_Flags and STYP_TEXT) /= 0
           and then Scn.S_Size > 0
         then
            Addr := Pc_Type (Scn.S_Vaddr) + Get_Image_Base (Exec.PE_File);
            Last := Addr + Get_Section_Length (Exec.PE_File, Idx) - 1;

            Item := new Address_Info'
              (Kind            => Section_Addresses,
               First           => Addr,
               Last            => Last,
               Top_Level       => <>,
               Parent          => null,
               Section_Name    => new String'(
                 Get_Section_Name (Exec.PE_File, Idx)),
               Section_Sec_Idx => Section_Index (Idx),
               Section_Content => Invalid_Binary_Content,
               Section_LS      => No_Loaded_Section);
            Insert_With_Top_Level_Update
              (Exec.Desc_Sets (Section_Addresses), Item);
         end if;
      end loop;
   end Build_Sections;

   --------------
   -- Get_Sloc --
   --------------

   function Get_Sloc
     (Set : Address_Info_Sets.Set;
      PC  : Pc_Type) return Source_Location
   is
      SL : constant Source_Locations :=
         Get_Slocs (Set, PC, Non_Empty_Only => True);
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
     (Set            : Address_Info_Sets.Set;
      PC             : Pc_Type;
      Non_Empty_Only : Boolean := False) return Source_Locations
   is
      Line_Infos : constant Address_Info_Arr :=
                           Get_Address_Infos (Set, Line_Addresses, PC);
      Result     : Source_Locations (1 .. Natural (Line_Infos'Length));
      Last       : Natural := Result'First - 1;

   begin
      for Addr_Info of Line_Infos loop
         if not Empty_Range (Addr_Info.all)
              and then
            (Addr_Info.Is_Non_Empty or else not Non_Empty_Only)
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
     (Exec                      : Exe_File_Type;
      PC                        : Pc_Type;
      CU_Filename, CU_Directory : out String_Access)
   is
      use Compile_Unit_Vectors;

      CU_Info : constant Address_Info_Acc :=
        Get_Address_Info (Exec, Compilation_Unit_Addresses, PC);
      CU_Id   : DIE_CU_Id := No_DIE_CU_Id;

   begin
      --  See if we match a compile unit first, then fallback on searching at
      --  the subprogram level.

      if CU_Info = null then
         declare
            Subp_Info : constant Address_Info_Acc :=
              Get_Address_Info (Exec, Subprogram_Addresses, PC);
         begin
            CU_Id :=
              (if Subp_Info = null
               then No_DIE_CU_Id
               else Subp_Info.Subprogram_DIE_CU);
         end;

      else
         CU_Id := CU_Info.DIE_CU;
      end if;

      if CU_Id = No_DIE_CU_Id then
         CU_Filename := null;
         CU_Directory := null;

      else
         declare
            CU : Compile_Unit_Desc renames
              Exec.Compile_Units.Element (CU_Id);
         begin
            CU_Filename := CU.Compile_Unit_Filename;
            CU_Directory := CU.Compilation_Directory;
         end;
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
           (Exec, Sec.Section_Sec_Idx,
            Len, Sec.Section_Content, Sec.Section_LS);
         Relocate (Sec.Section_Content, Sec.First);
      end if;
   end Load_Section_Content;

   --------------------------------------
   -- Load_Section_Content_And_Address --
   --------------------------------------

   function Load_Section_Content_And_Address
     (Exec    : Exe_File_Type'Class;
      Sec     : Section_Index;
      Len     : in out Elf_Addr;
      Content : in out Binary_Content;
      LS      : in out Loaded_Section;
      Addr    : in out Address)
     return Boolean
   is
   begin
      if not Is_Loaded (Content) then
         Alloc_And_Load_Section (Exec, Sec, Len, Content, LS);

         if not Is_Loaded (Content) then
            return False;
         end if;

         Addr := Address_Of (Content, 0);
      end if;

      return True;
   end Load_Section_Content_And_Address;

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
            Traces_Names.Add_Routine
              (Subp_Key, Exec, Sec.Section_Sec_Idx);

            begin
               Traces_Names.Add_Code_And_Traces
                 (Subp_Key,
                  Exec,
                  Sec.Section_Sec_Idx,
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

      First_Subp : constant Cursor := Find_Address_Info
        (Exec.Desc_Sets (Subprogram_Addresses),
         Subprogram_Addresses,
         Section.First);
      --  First subprogram to iterate on, used in Iterate_On_Lines. This is the
      --  subprogram with lowest start address that is strictly greater than
      --  Section.First - 1.

      Line_Counts : array (Source_File_Index
                           range Files_Table.First_File
                           .. Files_Table.Last_File)
        of Natural := (others => 0);
      --  For each source file, used to store the index of the last referenced
      --  line.

      procedure Iterate_On_Lines
        (Process : not null access procedure (Line_Info : Address_Info_Acc));
      --  Call Process on all lines from all subprograms in Section

      procedure Prealloc_Lines (Line_Info : Address_Info_Acc);
      --  Helper used to fill Line_Counts. Set the line count for corresponding
      --  file to the greatest one seen.

      procedure Build_Source_Line (Line_Info : Address_Info_Acc);
      --  Helper used to actually build source lines in internal data
      --  structures.

      ----------------------
      -- Iterate_On_Lines --
      ----------------------

      procedure Iterate_On_Lines
        (Process : not null access procedure (Line_Info : Address_Info_Acc))
      is
         Subprg_Cur : Cursor := First_Subp;
         Subprg     : Address_Info_Acc;
      begin
         --  If Find_Address_Info returned a cursor to an element that is
         --  before Section, then the first subprogram we are interested in
         --  (if any) is just after.

         if Has_Element (Subprg_Cur)
           and then Element (Subprg_Cur).First < Section.First
         then
            Next (Subprg_Cur);
         end if;

         while Has_Element (Subprg_Cur) loop
            Subprg := Element (Subprg_Cur);

            pragma Assert (Subprg.First >= Section.First);

            --  Stop on the first subprogram that is past Section (i.e. whose
            --  First address is in Section'Range).

            exit when Subprg.First > Section.Last;

            for Line of Subprg.Lines loop
               Process (Line);
            end loop;
            Next (Subprg_Cur);
         end loop;
      end Iterate_On_Lines;

      --------------------
      -- Prealloc_Lines --
      --------------------

      procedure Prealloc_Lines (Line_Info : Address_Info_Acc) is
         Sloc : Source_Location renames Line_Info.Sloc;
      begin
         Line_Counts (Sloc.Source_File) :=
           Natural'Max (Line_Counts (Sloc.Source_File), Sloc.L.Line);
      end Prealloc_Lines;

      -----------------------
      -- Build_Source_Line --
      -----------------------

      procedure Build_Source_Line (Line_Info : Address_Info_Acc) is
         Init_Line_State : constant Line_State :=
           (if Base = null
            then No_Code
            else Get_Line_State (Base.all, Line_Info.First, Line_Info.Last));
      begin
         Add_Line_For_Object_Coverage
           (Line_Info.Sloc.Source_File,
            Init_Line_State,

            --  ??? Debug info is sometimes attached to the line 0. This is
            --  authorized and specified in the DWARF standard, when an
            --  instruction should not be attached to any particular line. As
            --  we only support positive line indexes, we workaround it by
            --  attaching such instructions to the line 1. There likely won't
            --  be any code attached to line 1, so there is minimal risk of
            --  unrelated object code getting mixed up.
            --
            --  In the future, we should probably display such lines in their
            --  own section (for instance, specifically add a line indexed 0 to
            --  which we could attach such instructions, or have a specific
            --  section for it).

            (if Line_Info.Sloc.L.Line = 0 then 1 else Line_Info.Sloc.L.Line),
            Line_Info,
            Base,
            Exec);
      end Build_Source_Line;

   --  Start of processing for Build_Source_Lines_For_Section

   begin
      if Object_Coverage_Enabled then
         --  Optimisation: first preallocate line tables

         Iterate_On_Lines (Prealloc_Lines'Access);
         for File_Index in Line_Counts'Range loop
            if Line_Counts (File_Index) > 0 then
               Expand_Line_Table (File_Index, Line_Counts (File_Index));
            end if;
         end loop;

         --  Then actually build source lines

         Iterate_On_Lines (Build_Source_Line'Access);
      end if;
   end Build_Source_Lines_For_Section;

   --------------------
   -- Set_Insn_State --
   --------------------

   procedure Set_Insn_State
     (Base          : in out Traces_Base;
      Section       : Binary_Content;
      I_Ranges      : Insn_Set_Ranges;
      Last_Executed : out Pc_Type)
   is
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

      Cache          : Insn_Set_Cache := Empty_Cache;
      Next_Pc        : Pc_Type;
      --  Temporary instruction address, used only when looking for last
      --  instructions.

      Branch               : Branch_Kind;
      Flag_Indir           : Boolean;
      Branch_Dest, FT_Dest : Dest;
      --  Unused, but mandatory arguments when getting instructions properties

      Disa : access Disassembler'Class;

   --  Start of processing for Set_Insn_State

   begin
      Last_Executed := No_PC;

      Init_Post (Base, It, Section.First);
      Get_Next_Trace (Trace, It);

      while Trace /= Bad_Trace loop

         Last_Executed := Trace.Last;

         --  First, search the two last instructions (two to handle the delay
         --  slot if needed).

         Last_Pc_1 := No_PC;
         Last_Pc_2 := Trace.First;
         loop
            Disa := Disa_For_Machine (Machine, I_Ranges, Cache, Last_Pc_2);
            Last_Insn_2_Len := Pc_Type
              (Disa.Get_Insn_Length (Slice (Section, Last_Pc_2, Trace.Last)));
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
            Disa.Get_Insn_Properties
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
            Disa.Get_Insn_Properties
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

   ---------------------------
   -- Build_Insn_Set_Ranges --
   ---------------------------

   procedure Build_Insn_Set_Ranges
     (Exec            : in out Exe_File_Type'Class;
      Mapping_Symbols : Mapping_Symbol_Sets.Set;
      Section         : Address_Info_Acc)
   is
      Sec_Idx          : constant Section_Index := Section.Section_Sec_Idx;
      I_Ranges         : constant Insn_Set_Ranges_Acc := new Insn_Set_Ranges;

      Current_Insn_Set : Insn_Set_Type := Default;
      First_Addr       : Pc_Type := Section.First;

   begin
      Exec.Insn_Set_Ranges.Insert (Sec_Idx, I_Ranges);

      for Sym of Mapping_Symbols loop
         if Sym.Insn_Set /= Current_Insn_Set then
            if Current_Insn_Set /= Default
              and then Sym.Address /= First_Addr
            then
               Add_Range
                 (I_Ranges.all,
                  First_Addr, Sym.Address  - 1,
                  Current_Insn_Set);
            end if;
            First_Addr := Sym.Address;
            Current_Insn_Set := Sym.Insn_Set;
         end if;
      end loop;

      if Current_Insn_Set /= Default then
         Add_Range
           (I_Ranges.all,
            First_Addr, Section.Last,
            Current_Insn_Set);
      end if;
   end Build_Insn_Set_Ranges;

   -------------------
   -- Build_Symbols --
   -------------------

   procedure Build_Symbols (Exec : in out Elf_Exe_File_Type) is
      use Address_Info_Sets;

      type Addr_Info_Acc_Arr is array (0 .. Get_Shdr_Num (Exec.Elf_File))
        of Address_Info_Acc;
      Sections_Info : Addr_Info_Acc_Arr := (others => null);
      Sec : Address_Info_Acc;

      type Mapping_Symbols_Arr is array (0 .. Get_Shdr_Num (Exec.Elf_File))
        of Mapping_Symbol_Sets.Set;
      Mapping_Symbols : Mapping_Symbols_Arr;

      Symtab_Base : Address;
      Do_Reloc : Boolean;

      Strtab_Idx      : Elf_Half;
      Strtab_Len      : Elf_Addr;
      Strtabs         : Binary_Content;
      Strtabs_Section : Loaded_Section;
      ESym            : Elf_Sym;
      Offset          : Pc_Type;

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
         Sections_Info (Elf_Half (Sec.Section_Sec_Idx)) := Sec;
         Next (Cur);
      end loop;

      --  Load symtab and strtab

      if Exec.Sec_Symtab = SHN_UNDEF then
         return;
      end if;
      Load_Symtab (Exec);
      Symtab_Base := Address_Of (Exec.Symtab, 0);

      Strtab_Idx := Get_Strtab_Idx (Exec);
      if Strtab_Idx = SHN_UNDEF then
         return;
      end if;
      Alloc_And_Load_Section (Exec, Section_Index (Strtab_Idx),
                              Strtab_Len, Strtabs, Strtabs_Section);

      Do_Reloc := Get_Ehdr (Exec.Elf_File).E_Type = ET_REL;
      Offset := Exec.Exe_Text_Start;

      for I in 1 .. Exec.Nbr_Symbols loop
         ESym := Get_Sym
           (Exec.Elf_File,
            Symtab_Base + Storage_Offset ((I - 1) * Elf_Sym_Size));
         Sym_Type := Elf_St_Type (ESym.St_Info);

         if (Sym_Type = STT_FUNC or else Sym_Type = STT_NOTYPE)
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

            --  On ARM, the low address bit is used to distinguish ARM and
            --  Thumb instructions but it must be discarded when dealing
            --  with memory.

            if Get_Machine (Exec) = EM_ARM and (Sym.First and 1) = 1 then
               Sym.First := Sym.First - 1;
               Sym.Last := Sym.Last - 1;
            end if;

            Insert_With_Top_Level_Update
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

         if ELF_Machine = EM_ARM
           and then Sym_Type = STT_NOTYPE
           and then Elf_St_Bind (ESym.St_Info) = STB_LOCAL
         then
            declare
               Name : constant String := Read_String
                 (Address_Of (Strtabs, Elf_Addr (ESym.St_Name)));
            begin
               if Name'Length >= 2 and then Name (1) = '$' then

                  if ESym.St_Size /= 0 then
                     raise Program_Error with
                       "Illegal mapping symbol (size not null)";

                  elsif not (Name (2) in 'a' | 'd' | 't')
                    or else (Name'Length > 2
                             and then Name (3) /= '.')
                  then
                     raise Program_Error with
                       "Illegal mapping symbol (invalid name)";
                  end if;

                  declare
                     Mapping  : Mapping_Symbol_Sets.Set renames
                        Mapping_Symbols (ESym.St_Shndx);
                     Symbol   : constant Mapping_Symbol :=
                       ((Address  => ESym.St_Value,
                         Insn_Set => (case Name (2) is
                                      when 'a' => ARM,
                                      when 'd' => Data,
                                      when 't' => Thumb,
                                      when others => raise Program_Error)));
                     Position : Mapping_Symbol_Sets.Cursor;
                     Inserted : Boolean;
                  begin
                     Mapping.Insert (Symbol, Position, Inserted);

                     --  Having multiple mapping symbols at the same address
                     --  can happen: just check that they are not inconsistent.

                     if not Inserted
                          and then
                        Mapping_Symbol_Sets.Element (Position).Insn_Set
                          /= Symbol.Insn_Set
                     then
                        raise Program_Error with
                          ("Inconsistent mapping symbols at "
                           & Hex_Image (ESym.St_Value));
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      for Section_Index in Mapping_Symbols_Arr'Range loop
         if Sections_Info (Section_Index) /= null then
            Build_Insn_Set_Ranges
              (Exec,
               Mapping_Symbols (Section_Index),
               Sections_Info (Section_Index));
         end if;
      end loop;

      Free (Strtabs_Section);

      --  Query libbfd for synthetic symbols

      declare
         procedure For_Each_Synthetic_Symbol
           (Filename : chars_ptr;
            Callback : access procedure (Sym_Name  : chars_ptr;
                                         Sym_Value : Elf_Addr);
            Warn_Callback : access procedure (C_Msg : chars_ptr;
                                              C_BFD_Msg : chars_ptr));
         pragma Import (C, For_Each_Synthetic_Symbol,
                        "_gnatcov_for_each_synthetic_symbol");

         procedure Enter_Synthetic_Symbol
           (Sym_Name  : chars_ptr;
            Sym_Value : Elf_Addr) with Convention => C;
         --  Callback to enter a synthetic symbol into the symbol table

         procedure Warn (C_Msg : chars_ptr; C_BFD_Msg : chars_ptr)
           with Convention => C;
         --  Callback to process a warning from the BFD helpers

         ----------------------------
         -- Enter_Synthetic_Symbol --
         ----------------------------

         procedure Enter_Synthetic_Symbol
           (Sym_Name  : chars_ptr;
            Sym_Value : Elf_Addr)
         is
         begin
            Sym := new Address_Info'
              (Kind          => Symbol_Addresses,
               First         => Sym_Value,
               Last          => Sym_Value,
               Top_Level     => <>,
               Parent        => null,
               Symbol_Name   => new String'(Value (Sym_Name)),
               others        => <>);

            Insert_With_Top_Level_Update
              (Exec.Desc_Sets (Symbol_Addresses), Sym, Cur, Ok);
         end Enter_Synthetic_Symbol;

         ----------
         -- Warn --
         ----------

         procedure Warn (C_Msg : chars_ptr; C_BFD_Msg : chars_ptr) is
            use Diagnostics;
         begin
            Report
              (Value (C_Msg) & ": " & Value (C_BFD_Msg),
               Exe  => Exec'Unchecked_Access,
               Kind => Notice);
         end Warn;

         C_Filename : aliased Interfaces.C.char_array :=
           To_C (Exec.Get_Filename);

      begin
         For_Each_Synthetic_Symbol
           (To_Chars_Ptr (C_Filename'Unchecked_Access),
            Enter_Synthetic_Symbol'Access,
            Warn'Access);
      end;
   end Build_Symbols;

   -------------------
   -- Build_Symbols --
   -------------------

   procedure Build_Symbols (Exec : in out PE_Exe_File_Type) is
      use Address_Info_Sets;

      type Addr_Info_Acc_Arr is array (0 .. Get_Nbr_Sections (Exec.File.all))
        of Address_Info_Acc;
      Sections_Info : Addr_Info_Acc_Arr := (others => null);
      Sec : Address_Info_Acc;

      Sym : Address_Info_Acc;

      Cur : Cursor;
      Ok : Boolean;
   begin
      --  Build_Sections must be called before

      if Exec.Desc_Sets (Section_Addresses).Is_Empty then
         return;
      end if;

      if not Exec.Desc_Sets (Symbol_Addresses).Is_Empty then
         return;
      end if;

      --  Fill Sections_Info

      Cur := First (Exec.Desc_Sets (Section_Addresses));
      while Has_Element (Cur) loop
         Sec := Element (Cur);
         Sections_Info (Sec.Section_Sec_Idx + 1) := Sec;
         Next (Cur);
      end loop;

      declare
         Syms      : constant Loaded_Section := Get_Symbols (Exec.PE_File);
         Syms_Base : constant Address := Address_Of (Syms);
         Nbr_Syms  : constant Unsigned_32 :=
           Get_Hdr (Exec.PE_File).F_Nsyms;
         I         : Unsigned_32;
      begin
         I := 0;
         while I < Nbr_Syms loop
            declare
               use Coff;
               S : Syment;
               for S'Address use Syms_Base + Storage_Offset (I * Symesz);
               pragma Import (Ada, S);
            begin
               if S.E_Type = 16#20# then
                  --  A function
                  Sec := Sections_Info (Section_Index (S.E_Scnum));
                  if Sec /= null then
                     Sym := new Address_Info'
                       (Kind        => Symbol_Addresses,
                        First       => Sec.First + Pc_Type (S.E_Value),
                        Last        => Sec.First + Pc_Type (S.E_Value),
                        Top_Level   => True,
                        Parent      => Sec,
                        Symbol_Name => new String'
                        (Get_Symbol_Name (Exec.PE_File, S)),
                        others      => <>);

                     --  Address_Info ranges are built "artificially" so that
                     --  all symbols are contiguous. There is no need to keep
                     --  track of which one is top level or not since they all
                     --  are, and the "Tree condition" is automatically
                     --  respected because the setting of ranges in the
                     --  following loop ensures that no symbols overlap.

                     Exec.Desc_Sets (Symbol_Addresses).Insert (Sym, Cur, Ok);
                  end if;

               end if;

               I := I + 1 + Unsigned_32 (S.E_Numaux);
            end;
         end loop;
      end;

      --  Set range on symbols. We assume there is no hole: additional
      --  instructions are there for padding. Padding is an issue for
      --  consolidation, but we have a dedicated machinery to deal with it in
      --  Traces_Names.

      declare
         Prev : Address_Info_Acc;
         Prev_Cur : Cursor;
      begin
         Cur := First (Exec.Desc_Sets (Symbol_Addresses));
         if Has_Element (Cur) then
            Prev := Element (Cur);
            Prev_Cur := Cur;
            Next (Cur);
            while Has_Element (Cur) loop
               Sym := Element (Cur);
               if Prev.First = Sym.First then
                  --  Symbol is empty.  Remove it.
                  Address_Info_Sets.Delete
                    (Exec.Desc_Sets (Symbol_Addresses), Prev_Cur);
               else
                  Prev.Last := Sym.First - 1;
               end if;
               Prev := Sym;
               Prev_Cur := Cur;
               Next (Cur);
            end loop;
         end if;

         --  Don't forget the range for last symbol: span it until the end of
         --  the corresponding section.

         Cur := Last (Exec.Desc_Sets (Symbol_Addresses));
         if Has_Element (Cur) then
            Sym := Element (Cur);
            Sym.Last := Sym.Parent.Last;
         end if;
      end;
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
                     Get_Address_Infos (Set, Kind, PC, Innermost_Only => True);
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
   is (Get_Address_Info (Get_Desc_Set (Exec, Kind, PC).all, Kind, PC));

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
     (Set            : Address_Info_Sets.Set;
      Kind           : Address_Info_Kind;
      PC             : Pc_Type;
      Innermost_Only : Boolean := False) return Address_Info_Arr
   is
      use Address_Info_Sets;

      Cache : AI_Cache_Entry renames AI_Cache (Kind);

      Prev, Last : Cursor;
      Count      : Natural := 0;

   begin
      --  First check whether results for the last lookup still match

      if Cache.Last /= No_Element
           and then
         Set'Unrestricted_Access = Cache.Last_Set
           and then
         (PC in Cache.Last_Info.First .. Cache.Last_Info.Last
          or else PC = Cache.Last_Info.First)
      then
         --  We have a candidate match, whose range contains the PC we're
         --  looking for. Now move from there to make sure that we return
         --  the innermost match. At any given point in time, Cache.Last
         --  is our best candidate so far.

         declare
            Candidate : Cursor := Cache.Last;
         begin
            loop
               Next (Candidate);
               exit when Candidate = No_Element;
               declare
                  Candidate_Info : constant Address_Info_Acc :=
                    Element (Candidate);
               begin
                  if PC in Candidate_Info.First .. Candidate_Info.Last
                       or else
                     PC = Candidate_Info.First
                  then
                     Cache.Last      := Candidate;
                     Cache.Last_Info := Candidate_Info;
                  end if;

                  exit when PC < Candidate_Info.First;
               end;
            end loop;
         end;

         Bump (Addr_Map_Cache_Hit);

         --  The following assertion does the full costly lookup to double
         --  check that the cache is returning the correct information: it
         --  should not be enabled for production use, as it nullifies the
         --  entire benefit of the cache.

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

      while Prev /= No_Element loop
         declare
            Prev_Info : constant Address_Info_Acc := Element (Prev);
         begin
            exit when not (Prev_Info.First <= PC
                           and then (Prev_Info.First > Prev_Info.Last
                                     or else Prev_Info.Last >= PC));
         end;

         Count := Count + 1;
         exit when Innermost_Only;

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
     (Exec           : Exe_File_Type;
      Kind           : Address_Info_Kind;
      PC             : Pc_Type;
      Innermost_Only : Boolean := False) return Address_Info_Arr
   is
     (Get_Address_Infos
        (Get_Desc_Set (Exec, Kind, PC).all, Kind, PC, Innermost_Only));

   -------------------------------
   -- Tree_Assumption_Respected --
   -------------------------------

   function Tree_Assumption_Respected
     (Set  : Address_Info_Sets.Set;
      Item : Address_Info_Acc) return Boolean
   is
      use Address_Info_Sets;

      Init : constant Cursor := Set.Floor (Item);
      Cur : Cursor;

      function Contains (Outer, Inner : Address_Info_Acc) return Boolean
      is (Outer.First <= Inner.First and then Outer.Last >= Inner.Last);
      --  Returns whether the PC range of Outer contains the PC range of Inner

      function Nested_Or_Disjoint
        (Item, Other : Address_Info_Acc) return Boolean
      is
        (Empty_Range (Item.all)
         or else Empty_Range (Other.all)
         or else Contains (Item, Other)
         or else Contains (Other, Item)
         or else Item.First > Other.Last
         or else Item.Last < Other.First);
      --  Return whether Item and Other respect the assumption that either one
      --  contains the other, or the are disjoint. If one of the Address_Info
      --  has an empty range, we consider the assumption to be true.

   begin
      --  Thanks to the fact that all the couples of elements from Set satisfy
      --  the Tree_Assumption/Nested_Or_Disjoint, we don't need to check that
      --  Item satisfies it for every element in Set. Just check the predicate
      --  with the elements between the two top level entries surrounding Item
      --  is enough.

      if Empty_Range (Item.all) then
         return True;
      end if;

      --  Position Cur next to the actual position of Item

      Cur := Init;

      --  First check that for every Address_Info_Acc between Set.Floor(Item)
      --  and the previous top level entry, it is either disjoint from Item or
      --  one contains the other.

      while Has_Element (Cur) loop

         if not Nested_Or_Disjoint (Item, Element (Cur)) then
            return False;
         end if;

         exit when Element (Cur).Top_Level;
         Previous (Cur);

      end loop;

      --  Now do the same for all then entries after the actual position of
      --  Item. If Init has no element, this means that Item would be the first
      --  item of the set. In that case, start checking from Set.First.

      if Has_Element (Init) then
         Cur := Next (Init);
      else
         Cur := Set.First;
      end if;

      while Has_Element (Cur) loop

         if not Nested_Or_Disjoint (Item, Element (Cur)) then
            return False;
         end if;

         exit when Element (Cur).Top_Level;
         Next (Cur);

      end loop;

      return True;

   end Tree_Assumption_Respected;

   -----------------------------------
   -- Insert_With_Top_Level_Update --
   -----------------------------------

   procedure Insert_With_Top_Level_Update
     (Set      : in out Address_Info_Sets.Set;
      Item     : Address_Info_Acc;
      Pos      : out Address_Info_Sets.Cursor;
      Inserted : out Boolean)
   is
      use Address_Info_Sets;

      Prev_TL_Cur  : Cursor;
      Prev_TL_Info : Address_Info_Acc;
      Next_Item    : Address_Info_Acc;
   begin
      if not Tree_Assumption_Respected (Set, Item) then
         Outputs.Fatal_Error
           ("Unexpected address range for a "
            & (case Item.Kind is
                 when Compilation_Unit_Addresses   => "compilation Unit",
                 when Section_Addresses            => "section",
                 when Subprogram_Addresses         => "subprogram",
                 when Inlined_Subprogram_Addresses => "inlined subprogram",
                 when Symbol_Addresses             => "symbol",
                 when Line_Addresses               => "debug line")
            & " between " & Hex_Image (Item.First) & " and "
            & Hex_Image (Item.Last));
      end if;

      --  Start by inserting the new item in the set

      Set.Insert (Item, Pos, Inserted);

      if not Inserted then
         return;
      end if;

      --  Then, from the position of insertion of the new item, search for the
      --  previous top level entry, if it exists.

      Prev_TL_Cur := Previous (Pos);

      loop
         --  If we reach the begining of the set before finding a top level
         --  item (which should only happen if the item got inserted in the
         --  first position) then we must flag it as top level.

         if not Has_Element (Prev_TL_Cur) then
            pragma Assert (Item = Set.First_Element);
            Item.Top_Level := True;
            exit;
         end if;

         Prev_TL_Info := Element (Prev_TL_Cur);

         --  If we find a top level Address_Info, then we can check if our item
         --  is top level or not. Prev_TL_Info and Item are either nested
         --  or disjoint (I.E. Prev_TL_Info is the root of the tree of Item, or
         --  it is in another tree than Item). We have Prev_TL_Info <= Item
         --  thus we are in one of the two following situations :
         --
         --  [Prev_TL_Info ]    or  [Prev_TL_Info ]  [Item]
         --    [Item  ]
         --
         --  In the first case, the top level Address_Info of the tree of Item
         --  is indeed Prev_TL_Info, and Item must not be marked as Top_Level.
         --  In the second case, Prev_TL_Info is not in the same tree as Item,
         --  which means that Item is the root of its treee, and must be marked
         --  as Top_Level.

         if Prev_TL_Info.Top_Level then
            Item.Top_Level := Prev_TL_Info.Last < Item.First;
            exit;
         end if;

         --  We haven't found a top level Address_Info yet, keep iterating over
         --  the set.

         Previous (Prev_TL_Cur);
      end loop;

      Next_Item := (if Has_Element (Next (Pos))
                    then Element (Next (Pos))
                    else null);

      --  If the Address_Info we are inserting is top level, we may need to
      --  update the next element in the set depending on the nesting:
      --  we either have Next_Item included in Item or they are disjoint.
      --
      --  [Item          ]   or  [Item    ]  [Next_Item    ]
      --    [Next_Item ]
      --
      --  In the first case we need to make sure that Next_Item is not tagged
      --  as top level anymore.

      if Item.Top_Level
        and then Next_Item /= null
        and then Item.Last >= Next_Item.Last
      then
         Next_Item.Top_Level := False;
      end if;

   end Insert_With_Top_Level_Update;

   procedure Insert_With_Top_Level_Update
     (Set : in out Address_Info_Sets.Set; Item : in out Address_Info_Acc)
   is
      Pos           : Address_Info_Sets.Cursor;
      Inserted      : Boolean;
      Existing_Item : Address_Info_Acc;
   begin
      Insert_With_Top_Level_Update (Set, Item, Pos, Inserted);

      --  The call above will make gnatcov abort if Item breaks the address
      --  range nesting invariant, so Inserted will be False iff. we tried to
      --  insert a duplicate entry.

      if not Inserted then
         Existing_Item := Address_Info_Sets.Element (Pos);
         if Item.Kind = Subprogram_Addresses
            and then Item.First = Existing_Item.First
            and then Item.Last = Existing_Item.Last
            and then Item.Subprogram_Name /= null
            and then Existing_Item.Subprogram_Name /= null
            and then Item.Subprogram_Name.all
                     = Existing_Item.Subprogram_Name.all
         then
            Free (Item);
            Item := Existing_Item;

         else
            raise Constraint_Error
              with "cannot insert duplicate Address_Info: " & Image (Item);
         end if;
      end if;
   end Insert_With_Top_Level_Update;

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
      Buffer   : in out Highlighting.Buffer_Type)
   is
      use Highlighting;

      Symbol : constant Address_Info_Acc := Get_Symbol (Sym, Pc);

   --  Start of processing for Symbolize

   begin
      if Symbol = null then
         return;
      end if;

      Buffer.Start_Token (Text);
      Buffer.Put (' ');
      Buffer.Start_Token (Punctuation);
      Buffer.Put ('<');
      Buffer.Start_Token (Highlighting.Name);
      Buffer.Put (Symbol.Symbol_Name.all);
      if Pc /= Symbol.First then
         Buffer.Start_Token (Punctuation);
         Buffer.Put ('+');
         Buffer.Start_Token (Literal);
         Buffer.Put
           ("0x" & Strip_Zero_Padding (Hex_Image (Pc - Symbol.First)));
      end if;
      Buffer.Start_Token (Punctuation);
      Buffer.Put ('>');
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

      procedure Do_Set_Insn_State
        (Info          : in out Subprogram_Info;
         Last_Executed : out Pc_Type);
      --  Helper to invoke Set_Insn_Trace in Build_Routine_Insn_State

      ------------------------------
      -- Build_Routine_Insn_State --
      ------------------------------

      procedure Build_Routine_Insn_State
        (Key  : Subprogram_Key;
         Info : in out Subprogram_Info)
      is
         pragma Unreferenced (Key);

         Last_Executed : Pc_Type;
         Padding_Found : Boolean;
      begin
         if not Is_Loaded (Info.Insns) then
            return;
         end if;

         Do_Set_Insn_State (Info, Last_Executed);

         --  If there's a chance that padding NOPs yield pessimistic object
         --  coverage results (because they are never executed) for this
         --  subprogram, try to strip them (if not already done) and recompute
         --  the object coverage for it.

         if Last_Executed < Info.Insns.Last
              and then
            not Info.Padding_Stripped
              and then
            not Has_Precise_Symbol_Size (Info.Exec.all)
         then
            Strip_Padding (Info.Exec, Info.Section, Info.Insns, Padding_Found);
            Info.Padding_Stripped := True;
            if Padding_Found then
               Do_Set_Insn_State (Info, Last_Executed);
            end if;
         end if;
      end Build_Routine_Insn_State;

      -----------------------
      -- Do_Set_Insn_State --
      -----------------------

      procedure Do_Set_Insn_State
        (Info          : in out Subprogram_Info;
         Last_Executed : out Pc_Type)
      is
      begin
         Set_Insn_State
           (Info.Traces.all,
            Info.Insns,
            Get_Insn_Set_Ranges (Info.Exec.all, Info.Section).all,
            Last_Executed);
      end Do_Set_Insn_State;

   --  Start of processing for Build_Routines_Insn_State

   begin
      Iterate (Build_Routine_Insn_State'Access);
   end Build_Routines_Insn_State;

   --------------------------
   -- Disassemble_File_Raw --
   --------------------------

   procedure Disassemble_File_Raw (File : in out Exe_File_Type'Class) is
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

         I_Ranges : Insn_Set_Ranges renames
           Get_Insn_Set_Ranges (File, Sec.Section_Sec_Idx).all;
         Cache    : Insn_Set_Cache := Empty_Cache;

         Insns    : Binary_Content;
         Buffer   : Highlighting.Buffer_Type (128);
      begin
         Load_Section_Content (File, Sec);
         Put_Line ("section " & Sec.Section_Name.all);
         Insns := Sec.Section_Content;
         Pc := Insns.First;

         while Pc <= Insns.Last loop
            Put (Hex_Image (Pc));
            Put (":");
            Put (ASCII.HT);

            Buffer.Reset;
            Disa_For_Machine (Machine, I_Ranges, Cache, Pc).
              Disassemble_Insn
                (Slice (Insns, Pc, Insns.Last), Pc,
                 Buffer, Insn_Len, File);

            for I in Pc .. Pc + Pc_Type (Insn_Len - 1) loop
               Put (Hex_Image (Get (Insns, I)));
               Put (' ');
            end loop;

            for I in Insn_Len .. 3 loop
               Put ("   ");
            end loop;

            Put ("  ");
            Put (Buffer.Get_Raw);
            New_Line;

            Pc := Pc + Pc_Type (Insn_Len);
            exit when Pc = 0;
         end loop;
      end Local_Disassembler;

   --  Start of processing for Disassemble_File_Raw

   begin
      Build_Sections (File);
      Build_Symbols (File);
      File.Desc_Sets (Section_Addresses).Iterate (Local_Disassembler'Access);
   end Disassemble_File_Raw;

   ----------------------
   -- Disassemble_File --
   ----------------------

   procedure Disassemble_File (File : in out Exe_File_Type) is
      use Address_Info_Sets;
      Cur        : Cursor;
      Sec        : Address_Info_Acc;
      I_Ranges   : Insn_Set_Ranges_Cst_Acc;
      Addr       : Pc_Type;

      Cur_Subprg : Cursor;
      Subprg     : Address_Info_Acc;

      Cur_Symbol : Cursor;
      Symbol     : Address_Info_Acc;

      Last_Addr  : Pc_Type;
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
         I_Ranges := Get_Insn_Set_Ranges (File, Sec.Section_Sec_Idx);

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
                  and then (Subprg = null
                            or else Subprg.Subprogram_Name.all
                                    /= Symbol.Symbol_Name.all)
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
               I_Ranges.all,
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
      Cb       : access procedure (Exec : in out Exe_File_Type'Class));
   --  Call CB with an open executable file descriptor for FILE,
   --  closed on return.

   -----------------
   -- On_Elf_From --
   -----------------

   procedure On_Elf_From
     (Filename : String;
      Cb       : access procedure (Exec : in out Exe_File_Type'Class))
   is
      Exec : Exe_File_Acc;
   begin
      Open_Exec (Filename, 0, Exec); --  ??? Text_Start
      Cb (Exec.all);
      Close_Exec (Filename);
   exception
      when Binary_Files.Error =>
         Put_Line (Standard_Error, "cannot open: " & Filename);
         raise;
   end On_Elf_From;

   -----------------------
   -- Scan_Symbols_From --
   -----------------------

   procedure Scan_Symbols_From
     (File   : in out Exe_File_Type;
      Sym_Cb : access procedure (Sym : Address_Info_Acc);
      Strict : Boolean)
   is
      pragma Unreferenced (Strict);
      It : Addresses_Iterator;
      Sym : Address_Info_Acc;
      Sym_Copy : Address_Info (Symbol_Addresses);
   begin
      Build_Symbols (Exe_File_Type'Class (File));

      Init_Iterator (File, Symbol_Addresses, It);
      loop
         Next_Iterator (It, Sym);
         exit when Sym = null;
         Sym_Copy := Sym.all;
         Sym_Cb (Sym_Copy'Unrestricted_Access);
      end loop;
   end Scan_Symbols_From;

   procedure Scan_Symbols_From
     (File   : in out Elf_Exe_File_Type;
      Sym_Cb : access procedure (Sym : Address_Info_Acc);
      Strict : Boolean)
   is
      use Address_Info_Sets;

      Efile : Elf_File renames File.Elf_File;
      --  Corresponding Elf_File - as we do low level accesses

      Nbr_Shdr : constant Elf_Half := Get_Shdr_Num (Efile);

      type Set_Acc is access Address_Info_Sets.Set;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Address_Info_Sets.Set, Set_Acc);
      type Set_Acc_Array is array (0 .. Nbr_Shdr) of Set_Acc;

      Shdr_Sets : Set_Acc_Array := (others => null);
      --  Addresses container for each relevant sections

      Shdr   : Elf_Shdr_Acc;
      Last   : Pc_Type;
      Addr   : Pc_Type;
      Offset : Pc_Type;

      Sym     : Address_Info_Acc;
      Cur_Sym : Address_Info_Sets.Cursor;

      Symtab_Len      : Elf_Addr;
      Symtabs         : Binary_Content;
      Symtabs_Section : Loaded_Section;
      Symtab_Base     : Address;

      Strtab_Idx      : Elf_Half;
      Strtab_Len      : Elf_Addr;
      Strtabs         : Binary_Content;
      Strtabs_Section : Loaded_Section;

      A_Sym    : Elf_Sym;
      Sym_Name : String_Access;

      Sym_Type : Unsigned_8;
      Cur      : Address_Info_Sets.Cursor;
      Ok       : Boolean;
      Do_Reloc : Boolean;
   begin
      --  Search symtab and strtab, exit in case of failure

      if File.Sec_Symtab = SHN_UNDEF then
         Put_Line ("# No symbol table - file stripped ?");
         return;
      end if;
      Strtab_Idx := Get_Strtab_Idx (File);
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
           and then Shdr.Sh_Type = SHT_PROGBITS
           and then Shdr.Sh_Size > 0
         then
            Shdr_Sets (Idx) := new Address_Info_Sets.Set;
         end if;
      end loop;

      --  Load symtab and strtab

      Alloc_And_Load_Section (File, Section_Index (File.Sec_Symtab),
                              Symtab_Len, Symtabs, Symtabs_Section);
      Symtab_Base := Address_Of (Symtabs, 0);
      Alloc_And_Load_Section (File, Section_Index (Strtab_Idx),
                              Strtab_Len, Strtabs, Strtabs_Section);

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
                  Outputs.Warn
                    ("empty symbol " & Sym_Name.all
                     & " at " & Hex_Image (A_Sym.St_Value)
                     & " in section "
                     & Get_Shdr_Name (Efile, A_Sym.St_Shndx));
               end if;

               Free (Sym_Name);

            else
               declare
                  Sym_First : Pc_Type := Pc_Type (A_Sym.St_Value);
                  Sym_Last  : Pc_Type :=
                     Sym_First + Pc_Type (A_Sym.St_Size) - 1;
               begin
                  --  On ARM, the low address bit is used to distinguish ARM
                  --  and Thumb instructions but it must be discarded when
                  --  dealing with memory.

                  if Get_Machine (File) = EM_ARM and (Sym_First and 1) = 1 then
                     Sym_First := Sym_First - 1;
                     Sym_Last := Sym_Last - 1;
                  end if;

                  --  Non-empy symbol. Latch into our local container for
                  --  processing downstream.

                  Insert_With_Top_Level_Update
                    (Shdr_Sets (A_Sym.St_Shndx).all,
                     new Address_Info'
                       (Kind        => Symbol_Addresses,
                        First       => Sym_First,
                        Last        => Sym_Last,
                        Parent      => null,
                        Symbol_Name => Sym_Name,
                        others      => <>),
                     Cur, Ok);
               end;

               if not Ok then
                  Put_Line (Standard_Error,
                            "symbol " & Sym_Name.all
                              & " is an alias at "
                              & Hex_Image (A_Sym.St_Value));
               end if;

            end if;
         end if;
      end loop;

      Free (Strtabs_Section);
      Free (Symtabs_Section);

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
               Free (Sym);

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
                  Outputs.Warn
                    ("no symbols for "
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
               Free (Sym);

               Next (Cur_Sym);
               Sym := (if Has_Element (Cur_Sym)
                       then Element (Cur_Sym)
                       else null);
            end loop;

            if Strict and then Addr < Last then
               Outputs.Warn
                 ("no symbols for "
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

      procedure Process (Exec : in out Exe_File_Type'Class);

      procedure Process (Exec : in out Exe_File_Type'Class) is
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
     (File    : in out Exe_File_Type'Class;
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

            if not Traces_Names.Is_Routine_Of_Interest
               (Sym.Symbol_Name.all)
            then
               Traces_Names.Add_Routine_Of_Interest (Sym.Symbol_Name.all);
            end if;

            --  Scan_Symbols_From is going to free this instance of Sym, so
            --  take the ownership of the Symbol_Name string.

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

      procedure Process (Exec : in out Exe_File_Type'Class);
      --  Needs comment???

      -------------
      -- Process --
      -------------

      procedure Process (Exec : in out Exe_File_Type'Class) is
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

      Symbol_Cursor : Cursor := Exec.Desc_Sets (Symbol_Addresses).First;
      --  The closest symbol that matches the current line address.

      function Get_Symbol (Line : Address_Info_Acc) return Address_Info_Acc;
      --  Use Symbol to get efficiently the first symbol that matches Line.
      --  Return this symbol, or null if there is none.

      ----------------
      -- Get_Symbol --
      ----------------

      function Get_Symbol (Line : Address_Info_Acc) return Address_Info_Acc is
         Cur : Cursor := Symbol_Cursor;
         Sym : Address_Info_Acc;
      begin
         while Has_Element (Cur) loop
            Sym := Element (Cur);
            if Line.First < Sym.First then
               Diagnostics.Report
                 (Exec, Line.First,
                  "Source line belongs to no symbol",
                  Kind => Diagnostics.Warning);
               exit;

            elsif Line.First > Sym.Last then
               Next (Cur);

            else
               return Sym;
            end if;
         end loop;

         --  No symbol was found

         return null;
      end Get_Symbol;

      Line_Cursor   : Cursor;
      Sym_End_Addr  : aliased Address_Info (Line_Addresses);

      Cached_Symbol : Address_Info_Acc;
      Cached_Line   : Address_Info_Acc;
      --  Cached values for Element (Line_Cursor / Symbol_Cursor) to avoid
      --  costly calls to Address_Info_Sets.Element (Set, Cursor);

      Subp_Key      : Subprogram_Key;

   begin
      for Subprg of Exec.Desc_Sets (Subprogram_Addresses) loop
         Line_Cursor := First (Subprg.Lines);
         while Has_Element (Line_Cursor) loop
            Cached_Line := Element (Line_Cursor);

            --  Find the first symbol that matches this line address, if any.
            --  Get the first one that is just after otherwise.

            while Has_Element (Symbol_Cursor) loop
               Cached_Symbol := Element (Symbol_Cursor);
               exit when Cached_Line.First <= Cached_Symbol.Last;
               Next (Symbol_Cursor);
            end loop;

            declare
               Line_Addr        : constant Address_Info_Acc := Cached_Line;
               Sloc_Begin       : constant Source_Location := Line_Addr.Sloc;
               Sloc_End         : Source_Location;
               Next_Line_Cursor : Cursor;
               Next_Line_Addr   : Address_Info_Acc;
               Symbol           : constant Address_Info_Acc :=
                 Get_Symbol (Line_Addr);
               Select_Symbol    : Boolean;
            begin
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

                     if Get_Symbol (Next_Line_Addr) = Symbol then
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
                        Add_Routine
                          (Subp_Key,
                           Exec, Symbol.Parent.Section_Sec_Idx);
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

   -------------------------
   -- Get_Insn_Set_Ranges --
   -------------------------

   function Get_Insn_Set_Ranges
     (File    : Exe_File_Type;
      Section : Section_Index) return Insn_Set_Ranges_Cst_Acc
   is
      use Insn_Set_Ranges_Per_Section;

      Cur : constant Cursor := File.Insn_Set_Ranges.Find (Section);
   begin
      if Cur = No_Element then
         return No_Insn_Set_Ranges'Access;
      else
         return Insn_Set_Ranges_Cst_Acc (Element (Cur));
      end if;
   end Get_Insn_Set_Ranges;

   -----------------------------
   -- Has_Precise_Symbol_Size --
   -----------------------------

   function Has_Precise_Symbol_Size (File : Exe_File_Type) return Boolean is
   begin
      return File.File.all not in PE_File'Class;
   end Has_Precise_Symbol_Size;

   ------------------------
   -- Find_Padding_First --
   ------------------------

   function Find_Padding_First
     (Exec    : Exe_File_Acc;
      Section : Section_Index;
      Insns   : Binary_Content) return Pc_Type
   is
      Disas    : access Disassembler'Class;
      I_Ranges : Insn_Set_Ranges_Cst_Acc;
      Cache    : Insn_Set_Cache := Empty_Cache;
      Insn_Set : Insn_Set_Type;

      First_Padding : Pc_Type := Insns.Last + 1;
      PC            : Pc_Type := Insns.First;
      Insns_Slice   : Binary_Content;
      Insn_Len      : Pc_Type;
   begin
      I_Ranges := Get_Insn_Set_Ranges (Exec.all, Section);

      while Iterate_Over_Insns
        (I_Ranges.all, Cache, Insns.Last, PC, Insn_Set)
      loop
         Disas := Disa_For_Machine (Machine, Insn_Set);
         Insns_Slice := Slice (Insns, PC, Insns.Last);
         Insn_Len := Pc_Type (Disas.Get_Insn_Length (Insns_Slice));

         --  Invalid code may get us past the last byte available in Insns,
         --  without the above function to raise an error. Do complain if it's
         --  the case.

         if Insn_Len > Insns.Last - PC + 1 then
            Abort_Disassembler_Error
              (PC, Insns_Slice,
               "suspicious instruction is out of bounds");
         end if;

         --  As soon as we meet a non-padding instruction, pretend that the
         --  padding ones start right after.  As we are going through all of
         --  them, we wil get the real first one in the end.

         if not Disas.Is_Padding (Insns, PC) then
            First_Padding := PC + Insn_Len;
         end if;

         PC := PC + Insn_Len;
      end loop;
      return First_Padding;
   end Find_Padding_First;

   -------------------
   -- Strip_Padding --
   -------------------

   procedure Strip_Padding
     (Exec          : Exe_File_Acc;
      Section       : Section_Index;
      Insns         : in out Binary_Content;
      Padding_Found : out Boolean)
   is
      Padding_First : constant Pc_Type :=
         Find_Padding_First (Exec, Section, Insns) - 1;
   begin
      if Padding_First /= Insns.Last then
         Padding_Found := True;
         Insns.Last := Find_Padding_First (Exec, Section, Insns) - 1;
      else
         Padding_Found := False;
      end if;
   end Strip_Padding;

   ---------------------------------
   -- Platform_Independent_Symbol --
   ---------------------------------

   function Platform_Independent_Symbol
     (Name : String;
      File : Exe_File_Type) return String
   is
      Is_PE    : constant Boolean := File.File.all in PE_File'Class;
      Is_32bit : constant Boolean := Pc_Type_Size = 4;
   begin
      if Is_PE and then Is_32bit then
         pragma Assert (Name'Length > 0 and then Name (Name'First) = '_');
         return Name (Name'First + 1 .. Name'Last);

      elsif Ends_With (Name, "@plt") then
         return Name (Name'First .. Name'Last - 4);

      else
         return Name;
      end if;
   end Platform_Independent_Symbol;

end Traces_Elf;
