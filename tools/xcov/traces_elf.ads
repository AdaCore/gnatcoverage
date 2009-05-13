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

with Ada.Unchecked_Deallocation;
with Traces; use Traces;
with Traces_Dbase; use Traces_Dbase;
with Elf_Arch;     use Elf_Arch;
with Elf_Common;   use Elf_Common;
with Elf_Files;    use Elf_Files;
with Strings;      use Strings;

with Interfaces;
with Ada.Containers.Ordered_Sets;
with System; use System;
with Disa_Symbolize; use Disa_Symbolize;

package Traces_Elf is

   type Binary_Content is
     array (Elf_Arch.Elf_Size range <>) of Interfaces.Unsigned_8;
   --  An array of byte, used to store ELF sections

   type Binary_Content_Acc is access Binary_Content;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Binary_Content, Binary_Content_Acc);

   --  Executable file type.
   --  Extracted information are stored into such object.
   type Exe_File_Type is limited new Symbolizer with private;

   type Exe_File_Acc is access all Exe_File_Type;

   --  Makes symbolize non-abstract.
   procedure Symbolize (Sym : Exe_File_Type;
                        Pc : Traces.Pc_Type;
                        Line : in out String;
                        Line_Pos : in out Natural);

   --  Open an ELF file.
   --  TEXT_START is the offset of .text section.
   --  Exception Elf_Files.Error is raised in case of error.
   procedure Open_File
     (Exec : out Exe_File_Type; Filename : String; Text_Start : Pc_Type);

   --  Close the ELF file.
   --  The resources are still present but nothing anymore can be read from
   --  the file.
   procedure Close_Exe_File (Exec : in out Exe_File_Type);

   --  Close file and free built informations.
   procedure Close_File (Exec : in out Exe_File_Type);

   --  Get the filename of Exec.
   function Get_Filename (Exec : Exe_File_Type) return String;

   --  Get the machine type (ELF machine id).
   function Get_Machine (Exec : Exe_File_Type) return Interfaces.Unsigned_16;

   type Addresses_Info;
   type Addresses_Info_Acc is access all Addresses_Info;

   --  Build sections map for the current ELF file.
   procedure Build_Sections (Exec : in out Exe_File_Type);

   --  Load the content of a section.
   procedure Load_Section_Content (Exec : Exe_File_Type;
                                   Sec : Addresses_Info_Acc);

   --  Fill Traces_Names with traces from BASE.
   procedure Add_Subprograms_Traces (Exec : Exe_File_Acc; Base : Traces_Base);

   --  Using the executable, correctly set the state of every traces.
   procedure Set_Trace_State (Exec : Exe_File_Type; Base : in out Traces_Base);
   procedure Set_Trace_State
     (Base : in out Traces_Base; Section : Binary_Content);

   --  Read dwarfs info to build compile_units/subprograms lists.
   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type);

   procedure Build_Symbols (Exec : Exe_File_Acc);
   --  Read ELF symbol table.

   --  Read dwarfs info to build lines list.
   procedure Build_Debug_Lines (Exec : in out Exe_File_Type);

   --  Build source lines for a section of an exec and only for this section
   --  How are traces relevant (Base)???
   procedure Build_Source_Lines (Exec : Exe_File_Acc;
                                 Base : Traces_Base_Acc;
                                 Section : Binary_Content);

   procedure Disp_Address (El : Addresses_Info_Acc);
   function Image (El : Addresses_Info_Acc) return String;
   --  Display or return information about El (for debugging purposes)

   type Addresses_Kind is
     (Section_Addresses,
      Compile_Unit_Addresses,
      Subprogram_Addresses,
      Symbol_Addresses,
      Line_Addresses);

   procedure Disp_Addresses (Exe : Exe_File_Type; Kind : Addresses_Kind);
   --  Display the list of addresses for items of the indicated Kind in Exe

   function Get_Address_Info
     (Exec : Exe_File_Type;
      Kind : Addresses_Kind;
      PC   : PC_Type) return Addresses_Info_Acc;
   --  Retrieve the descriptor of the given Kind whose range contains address
   --  PC in Exec.

   function Get_Symbol (Exec : Exe_File_Type; Pc : Pc_Type)
                       return Addresses_Info_Acc;
   --  Short-hand for Get_Address_Info (Exec, Symbol_Address, PC)

   --  Canonical use of iterators:
   --
   --  Init_Iterator (Exe, Section_Addresses, It);
   --  loop
   --     Next_Iterator (It, Sec);
   --     exit when Sec = null;
   --     ...
   --  end loop;

   type Addresses_Iterator is limited private;

   procedure Init_Iterator (Exe : Exe_File_Type;
                            Kind : Addresses_Kind;
                            It : out Addresses_Iterator);

   procedure Next_Iterator (It : in out Addresses_Iterator;
                            Addr : out Addresses_Info_Acc);

   function "<" (L, R : Addresses_Info_Acc) return Boolean;

   package Addresses_Containers is new Ada.Containers.Ordered_Sets
     (Element_Type => Addresses_Info_Acc);

   type Addresses_Info (Kind : Addresses_Kind := Section_Addresses) is record
      --  Range of the info.
      First, Last : Traces.Pc_Type;

      --  Sections have no parent.
      --  Parent of a symbol is a section.
      --  Parent of a CU is a section.
      --  Parent of a subprogram is a CU.
      --  Parent of a line is a subprogram or a CU.
      Parent : Addresses_Info_Acc;

      case Kind is
         when Section_Addresses =>
            Section_Name : String_Acc;
            Section_Index : Elf_Common.Elf_Half;
            Section_Content : Binary_Content_Acc;

         when Compile_Unit_Addresses =>
            Compile_Unit_Filename : String_Acc;
            Compilation_Directory : String_Acc;
            Stmt_List             : Interfaces.Unsigned_32;

         when Subprogram_Addresses =>
            Subprogram_Name : String_Acc;

         when Symbol_Addresses =>
            Symbol_Name : String_Acc;

         when Line_Addresses =>
            Line_Filename : String_Acc;
            Line_Number   : Natural;
            Column_Number : Natural;
      end case;
   end record;

   procedure Unchecked_Deallocation is
      new Ada.Unchecked_Deallocation (Addresses_Info, Addresses_Info_Acc);

   procedure Read_Routines_Name
     (Filename : String; Exclude : Boolean; Keep_Open : Boolean);
   procedure Read_Routines_Name (Exec : Exe_File_Acc; Exclude : Boolean);
   --  Add (or remove if EXCLUDE is true) routines read from an ELF image
   --  to the routines database. If Keep_Open is True, leave the ELF image
   --  open after loading.
   --  Display errors on standard error.

   procedure Build_Source_Lines;
   --  Go through the routine database and, for each routine, populate the
   --  source database with the routine's source information.

   procedure Build_Routines_Trace_State;
   --  Go through the routine database and, for each routine, compute the
   --  state of its trace. This should be used only when the subroutine
   --  database has been populated with its traces.

   procedure Disassemble_File_Raw (File : in out Exe_File_Type);
   --  Simply disassemble the file.  Debug subprogram.

   procedure Disassemble_File (File : in out Exe_File_Type);
   --  Disassemble file with labels.  Debug subprogram.

private

   type Desc_Sets_Type is array (Addresses_Kind) of Addresses_Containers.Set;

   type Exe_File_Type is limited new Symbolizer with record
      --  Sections index.
      Sec_Debug_Abbrev   : Elf_Half := 0;
      Sec_Debug_Info     : Elf_Half := 0;
      Sec_Debug_Info_Rel : Elf_Half := 0;
      Sec_Debug_Line     : Elf_Half := 0;
      Sec_Debug_Line_Rel : Elf_Half := 0;
      Sec_Debug_Str      : Elf_Half := 0;
      Sec_Debug_Ranges   : Elf_Half := 0;

      Exe_File : Elf_Files.Elf_File;
      Exe_Text_Start : Elf_Addr;
      Exe_Machine : Elf_Half;
      Is_Big_Endian : Boolean;

      --  FIXME.
      Addr_Size : Natural := 0;

      Debug_Str_Base : Address := Null_Address;
      Debug_Str_Len : Elf_Size;
      Debug_Strs : Binary_Content_Acc;

      --  .debug_lines content.
      Lines_Len : Elf_Size := 0;
      Lines : Binary_Content_Acc := null;

      Desc_Sets : Desc_Sets_Type;
      --  Address descriptor sets
   end record;

   type Addresses_Iterator is limited record
      Cur : Addresses_Containers.Cursor;
   end record;

end Traces_Elf;
