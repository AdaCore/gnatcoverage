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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Interfaces; use Interfaces;

with System; use System;

with GNAT.OS_Lib;
with GNAT.Strings;     use GNAT.Strings;
with GNATCOLL.Mmap;    use GNATCOLL.Mmap;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;

with Arch;           use Arch;
with Binary_Files;   use Binary_Files;
with Disa_Symbolize; use Disa_Symbolize;
with Elf_Common;     use Elf_Common;
with Elf_Disassemblers; use Elf_Disassemblers;
with Elf_Files;      use Elf_Files;
with Highlighting;
with PECoff_Files;   use PECoff_Files;
with SC_Obligations; use SC_Obligations;
with Slocs;          use Slocs;
with Symbols;        use Symbols;
with Traces;         use Traces;
with Traces_Dbase;   use Traces_Dbase;
with Traces_Files;   use Traces_Files;

package Traces_Elf is

   type Exe_File_Type is abstract limited new Symbolizer with private;
   type Exe_File_Acc is access all Exe_File_Type'Class;
   --  Executable file type.
   --  Extracted information are stored into such object.

   overriding procedure Symbolize
     (Sym      : Exe_File_Type;
      Pc       : Traces.Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type);
   --  Makes symbolize non-abstract

   function Open_File
     (Filename   : String;
      Text_Start : Pc_Type)
      return Exe_File_Acc;
   --  Open an ELF file.
   --  TEXT_START is the offset of .text section.
   --  Exception Elf_Files.Error is raised in case of error.

   procedure Close_File (Exec : in out Exe_File_Acc);
   --  Close file and free built informations

   procedure Apply_Relocations
     (Exec    : in out Exe_File_Type;
      Sec_Idx : Section_Index;
      Region  : in out Mapped_Region;
      Data    : in out Binary_Content) is abstract;
   --  Apply relocations from SEC_REL to DATA.
   --  This procedure should only be called to relocate dwarf debug sections,
   --  and therefore handles only a small subset of the relocations. DATA must
   --  be a writable memory area.

   function Get_Filename (Exec : Exe_File_Type) return String;
   --  Get the filename of Exec

   function Get_Machine (Exec : Exe_File_Type) return Interfaces.Unsigned_16;
   --  Get the machine type (ELF machine id)

   function Get_Size (Exec : Exe_File_Type) return Long_Integer;
   --  Get the size of the Exec file

   function Get_Time_Stamp (Exec : Exe_File_Type) return GNAT.OS_Lib.OS_Time;
   --  Get the time stamp of the Exec file

   function Get_CRC32 (Exec : Exe_File_Type) return Unsigned_32;
   --  Get the CRC32 checksum of the content of the Exec file

   function Time_Stamp_Image (TS : GNAT.OS_Lib.OS_Time) return String;
   --  Return a simple string representation of a timestamp

   function Match_Trace_Executable
     (Exec : Exe_File_Type'Class; Trace_File : Trace_File_Type)
     return String;
   --  If the given executable file does not match the executable used to
   --  produce the given trace file, return why. Return an empty string
   --  otherwise.

   type Address_Info;
   type Address_Info_Acc is access all Address_Info;
   type Address_Info_Arr is array (Positive range <>) of Address_Info_Acc;

   procedure Build_Sections (Exec : in out Exe_File_Type) is abstract;
   --  Build sections map for the current ELF file

   procedure Load_Section_Content
     (Exec : Exe_File_Type;
      Sec  : Address_Info_Acc);
   --  Load the content of a section

   procedure Load_Code_And_Traces
     (Exec : Exe_File_Acc;
      Base : access Traces_Base);
   --  Load code for all symbols in Exec. If Base is not null, also load the
   --  traces into the routine database.

   procedure Set_Insn_State
     (Base          : in out Traces_Base;
      Section       : Binary_Content;
      I_Ranges      : Insn_Set_Ranges;
      Last_Executed : out Pc_Type);
   --  Using information from traces in Base and given the corresponding
   --  I_Ranges mapping, compute the state (i.e.  coverage) for instructions in
   --  Section. Set Last_Executed to the last PC executed (i.e. the PC for the
   --  last byte of the last executed instruction).

   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type'Class);
   --  Read DWARF info to build compile_units/subprograms lists

   procedure Build_Symbols (Exec : in out Exe_File_Type) is abstract;
   --  Read symbol table

   procedure Build_Debug_Lines (Exec : in out Exe_File_Type'Class);
   --  Read dwarfs info to build lines list

   procedure Build_Source_Lines_For_Section
     (Exec    : Exe_File_Acc;
      Base    : Traces_Base_Acc;
      Section : Binary_Content);
   --  Build source lines for a specific section of Exec
   --  If Base is not null, line state is initialized with object coverage
   --  status for each line.

   procedure Disp_Address (El : Address_Info_Acc);
   function Image (El : Address_Info_Acc) return String;
   --  Display or return information about El (for debugging purposes)

   type Address_Info_Kind is
     (Compilation_Unit_Addresses,
      Section_Addresses,
      Subprogram_Addresses,
      Symbol_Addresses,
      Line_Addresses);

   procedure Disp_Addresses (Exe : Exe_File_Type; Kind : Address_Info_Kind);
   --  Display the list of addresses for items of the indicated Kind in Exe

   procedure Disp_Compilation_Units (Exec : Exe_File_Type);
   --  Display compilation units filename of Exec

   --  Canonical use of iterators:
   --
   --  Init_Iterator (Exe, Section_Addresses, It);
   --  loop
   --     Next_Iterator (It, Sec);
   --     exit when Sec = null;
   --     ...
   --  end loop;

   type Addresses_Iterator is limited private;

   procedure Init_Iterator
     (Exe  : Exe_File_Type;
      Kind : Address_Info_Kind;
      It   : out Addresses_Iterator);

   procedure Next_Iterator
     (It   : in out Addresses_Iterator;
      Addr : out Address_Info_Acc);

   function "<" (L, R : Address_Info_Acc) return Boolean;
   --  Compare L and R by start address order in designated Address_Info
   --  record. For records with the same start address, compare names (for
   --  sections, subprograms or symbols) or slocs (for sloc info), with unset
   --  (null / No_Location) values sorting higher than any specific set value.
   --  Note that the end address is not part of the comparison key.

   package Address_Info_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Address_Info_Acc);

   type DIE_CU_Id is new Natural;
   No_DIE_CU_Id : constant DIE_CU_Id := 0;
   subtype Valid_DIE_CU_Id is
      DIE_CU_Id range No_DIE_CU_Id + 1 ..  DIE_CU_Id'Last;

   type Address_Info (Kind : Address_Info_Kind) is record
      --  Range of the info

      First, Last : Traces.Pc_Type;
      Parent      : Address_Info_Acc;

      --  Note: this is NOT the parent node in the sense of the DWARF tree.

      --  Instead, in this structure:
      --    Compilation units and sections have no parent.
      --    Parent of a symbol is a section.
      --    Parent of a CU is a section.
      --    Parent of a subprogram is a section as well
      --    Parent of a line is a subprogram or a CU.

      case Kind is
         when Compilation_Unit_Addresses =>
            DIE_CU : DIE_CU_Id;
            --  Compilation Unit from the DWARF information, used for
            --  consolidation.

         when Section_Addresses =>
            Section_Name    : String_Access;
            Section_Sec_Idx : Section_Index;
            Section_Content : Binary_Content;
            Section_Region  : Mapped_Region;

         when Subprogram_Addresses =>
            Subprogram_Name   : String_Access;
            Subprogram_CU     : CU_Id;
            --  Compilation Unit (in the LI file sense) for SCOs

            Subprogram_DIE_CU : DIE_CU_Id;
            --  Compilation Unit (in the DWARF sense) for consolidation.
            --  Associate one to each subprogram whenever possible since
            --  CU DIEs may not be a single memory block.

            Lines             : aliased Address_Info_Sets.Set;
            --  Line_Addresses info for this subprogram

         when Symbol_Addresses =>
            Symbol_Name   : String_Access;
            Symbol_Origin : Natural := 0;

         when Line_Addresses =>
            Sloc : Source_Location := No_Location;
            Disc : Unsigned_32     := 0;

            Is_Non_Empty : Boolean := False;
            --  Set True for the (only one) of a set of slocs associated with a
            --  given start address that has a non-empty address range.

      end case;
   end record;

   function Empty_Range (Info : Address_Info) return Boolean
     is (Traces.Empty_Range (Info.First, Info.Last));
   --  True if Info has an empty PC range

   function Find_Address_Info
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Sets.Cursor;
   pragma Inline (Find_Address_Info);
   --  Find cursor for address set entry containing PC

   function Find_Address_Info
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Sets.Cursor;
   --  Likewise, seeking the right address set depending on Kind

   function Get_Address_Info
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Acc;
   --  Retrieve the descriptor of the given Kind whose range contains address
   --  PC in Exec.

   function Get_Address_Infos
     (Exec : Exe_File_Type;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Arr;
   --  Same as Get_Address_Info, but return an array of matches

   function Get_Address_Info
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Acc;
   --  Retrieve the descriptor from the given Set whose range contains address
   --  PC in Exec.

   function Get_Address_Infos
     (Set  : Address_Info_Sets.Set;
      Kind : Address_Info_Kind;
      PC   : Pc_Type) return Address_Info_Arr;
   --  Same as Get_Address_Info, but return an array of matches

   function Get_Symbol
     (Exec : Exe_File_Type;
      PC   : Pc_Type) return Address_Info_Acc;
   --  Short-hand for Get_Address_Info (Exec, Symbol_Address, PC)

   function Get_Slocs
     (Set            : Address_Info_Sets.Set;
      PC             : Pc_Type;
      Non_Empty_Only : Boolean := False) return Source_Locations;
   --  Use Exec's debug_lines information to determine the slocs for the
   --  instruction at PC.

   function Get_Sloc
     (Set : Address_Info_Sets.Set;
      PC  : Pc_Type) return Source_Location;
   --  Same as Get_Slocs, but returning a unique source location, with a
   --  non-empty range.

   function Get_Call_Target
     (Exec     : Exe_File_Type;
      PC       : Pc_Type;
      Call_Len : Pc_Type) return Pc_Type;
   --  Return the target address of a call instruction from the debug
   --  information, or No_PC if there is no such information. PC must be the
   --  address of the first byte of the call instruction, and Call_Len its
   --  length.

   procedure Get_Compile_Unit
     (Exec                      : Exe_File_Type;
      PC                        : Pc_Type;
      CU_Filename, CU_Directory : out String_Access);
   --  Retrieve the filename/compilation directory of the compile unit that
   --  match the PC address, or set CU_* to null if no one matches.

   procedure Scan_Symbols_From
     (File   : in out Exe_File_Type;
      Sym_Cb : access procedure (Sym : Address_Info_Acc);
      Strict : Boolean);
   procedure Scan_Symbols_From
     (Filename : String;
      Sym_Cb   : access procedure (Sym : Address_Info_Acc);
      Strict   : Boolean);
   --  Scan FILE/FILENAME for executable symbols, calling SYM_CB for each
   --  unless SYM_CB is null. If STRICT, emit warnings about empty symbols and
   --  orphan regions on standard error. This is the one case when calling
   --  with a null SYM_CB is useful.

   procedure Read_Routine_Names
     (Filename  : String;
      Exclude   : Boolean;
      Strict    : Boolean);
   procedure Read_Routine_Names
     (File    : in out Exe_File_Type'Class;
      Exclude : Boolean;
      Strict  : Boolean := False);
   --  Add (or remove if EXCLUDE is true) routines read from an ELF image to
   --  the routine names to be covered. Honor STRICT as in Scan_Symbols_From
   --  and emit error about symbols defined twice on standard error as well.

   procedure Routine_Names_From_Lines
     (Exec     : Exe_File_Acc;
      Selected : not null access
                   function (Sloc_Begin : Source_Location;
                             Sloc_End   : Source_Location) return Boolean);
   --  Add routines read from an ELF image to the routines database. A
   --  routine is added iff at least one source line in the routine is
   --  selected by the filter (i.e. if Selected returns True for this line).

   procedure Build_Source_Lines;
   --  Go through the routine database and, for each routine, populate the
   --  source database with the routine's source information.

   procedure Build_Routines_Insn_State;
   --  Go through the routine database and, for each routine, compute the
   --  state of its trace.
   --  What is "the trace of a routine"???
   --  This should be used only when the subroutine
   --  database has been populated with its traces.

   procedure Disassemble_File_Raw (File : in out Exe_File_Type'Class);
   --  Disassemble the file (for debugging purposes)

   procedure Disassemble_File (File : in out Exe_File_Type);
   --  Disassemble file with labels (for debugging purposes)

   function Get_Insn_Set_Ranges
     (File    : Exe_File_Type;
      Section : Section_Index) return Insn_Set_Ranges_Cst_Acc;
   --  Return an Insn_Set_Ranges that describes Section

   function Has_Precise_Symbol_Size (File : Exe_File_Type) return Boolean
     with Inline => True;
   --  Return whether File contains precise sizes for symbols.  This is true
   --  for ELF binaries but false for PE-COFF ones.
   --
   --  When we don't have precise symbol size information, we approximate the
   --  size of routines as if they spanned until the next routine (i.e. as if
   --  there was no padding between routines).  This is good enough in most
   --  cases but sometimes (for instance in object coverage) we need to peform
   --  more expensive computations in order to recover a more accurate proper
   --  symbol size.
   --
   --  This function is used to know whether it is necessary to perform these
   --  computations.

   function Find_Padding_First
     (Exec    : Exe_File_Acc;
      Section : Section_Index;
      Insns   : Binary_Content) return Pc_Type;
   --  Find the address of the first padding instruction in Insns. Padding
   --  instructions are consecutive effect-less instruction at the end of
   --  the routine.  If there is no padding instruction in Insn, return
   --  Insn.Last + 1.

   procedure Strip_Padding
     (Exec          : Exe_File_Acc;
      Section       : Section_Index;
      Insns         : in out Binary_Content;
      Padding_Found : out Boolean);
   --  Assuming Insns contains instructions coming from Section in Exec, update
   --  it in order to remove any padding instruction it might contain. Set
   --  Padding_Found to whether we found padding instructions.

   function Platform_Independent_Symbol
     (Name : String;
      File : Exe_File_Type) return String;
   --  Return a platform-independant symbol name for Name. This is used to hide
   --  differences between symbol names from PE on Windows (all prefixed with
   --  an underscore) and symbol names on other platforms.

private

   type Compile_Unit_Desc is record
      Compile_Unit_Filename : String_Access;
      Compilation_Directory : String_Access;
      Stmt_List             : Interfaces.Unsigned_32;
      Pc_Low                : Pc_Type;
      Pc_High               : Pc_Type;
   end record;

   package Compile_Unit_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Valid_DIE_CU_Id,
      Element_Type => Compile_Unit_Desc);

   package Call_Site_To_Target_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Pc_Type);

   package Symbol_To_PC_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbol,
      Element_Type => Pc_Type);

   package Insn_Set_Ranges_Per_Section is new Ada.Containers.Ordered_Maps
     (Key_Type     => Section_Index,
      Element_Type => Insn_Set_Ranges_Acc);

   type Desc_Sets_Type is
     array (Address_Info_Kind
            range Compilation_Unit_Addresses .. Symbol_Addresses)
     of aliased Address_Info_Sets.Set;
   --  Note: line addresses are stored within the enclosing Symbol entry

   type Binary_File_Acc is access all Binary_File'Class;

   type File_Kind is (File_Executable, File_Object, File_Others);

   type Exe_File_Type is abstract limited new Symbolizer with record
      File                : Binary_File_Acc;
      Kind                : File_Kind;

      Exe_Text_Start      : Elf_Addr;
      Exe_Machine         : Elf_Half;
      Is_Big_Endian       : Boolean;

      Sec_Debug_Abbrev    : Section_Index := No_Section;
      Sec_Debug_Info      : Section_Index := No_Section;
      Sec_Debug_Line      : Section_Index := No_Section;
      Sec_Debug_Str       : Section_Index := No_Section;
      Sec_Debug_Ranges    : Section_Index := No_Section;

      --  FIXME
      --  What is there to fix???
      Addr_Size           : Natural := 0;

      Debug_Str_Base      : Address := Null_Address;
      Debug_Str_Len       : Elf_Addr;
      Debug_Strs          : Binary_Content := Invalid_Binary_Content;
      Debug_Strs_Region   : Mapped_Region := Invalid_Mapped_Region;

      --  .debug_lines contents

      Lines_Len           : Elf_Addr := 0;
      Lines               : Binary_Content := Invalid_Binary_Content;
      Lines_Region        : Mapped_Region := Invalid_Mapped_Region;

      --  Symbol table

      Symtab              : Binary_Content := Invalid_Binary_Content;
      Symtab_Region       : Mapped_Region := Invalid_Mapped_Region;
      Nbr_Symbols         : Natural := 0;
      Symbol_To_PC        : Symbol_To_PC_Maps.Map;

      Compile_Units       : Compile_Unit_Vectors.Vector;
      --  Compilation units

      Call_Site_To_Target : Call_Site_To_Target_Maps.Map;

      Desc_Sets           : Desc_Sets_Type;
      --  Address descriptor sets

      Insn_Set_Ranges     : Insn_Set_Ranges_Per_Section.Map;
      --  For each section, a set of associations: address range -> instruction
      --  set; see Elf_Disassemblers.
   end record;

   procedure Close_Exe_File (Exec : in out Exe_File_Type);
   --  Prepare Exec for closing/deallocation
   --  The resources are still present but nothing anymore can be read from
   --  the file.

   type Elf_Exe_File_Type is limited new Exe_File_Type  with record
      Elf_File : aliased Elf_Files.Elf_File;

      --  Sections index

      Sec_Symtab          : Elf_Half := SHN_UNDEF;
   end record;

   procedure Close_Exe_File (Exec : in out Elf_Exe_File_Type);
   procedure Build_Symbols (Exec : in out Elf_Exe_File_Type);
   procedure Build_Sections (Exec : in out Elf_Exe_File_Type);
   procedure Apply_Relocations
     (Exec    : in out Elf_Exe_File_Type;
      Sec_Idx : Section_Index;
      Region  : in out Mapped_Region;
      Data    : in out Binary_Content);
   procedure Scan_Symbols_From
     (File   : in out Elf_Exe_File_Type;
      Sym_Cb : access procedure (Sym : Address_Info_Acc);
      Strict : Boolean);

   type PE_Exe_File_Type is limited new Exe_File_Type  with record
      PE_File : aliased PECoff_Files.PE_File;
   end record;

   procedure Close_Exe_File (Exec : in out PE_Exe_File_Type);
   procedure Build_Symbols (Exec : in out PE_Exe_File_Type);
   procedure Build_Sections (Exec : in out PE_Exe_File_Type);
   procedure Apply_Relocations
     (Exec    : in out PE_Exe_File_Type;
      Sec_Idx : Section_Index;
      Region  : in out Mapped_Region;
      Data    : in out Binary_Content);

   type Addresses_Iterator is limited record
      Cur : Address_Info_Sets.Cursor;
   end record;

end Traces_Elf;
