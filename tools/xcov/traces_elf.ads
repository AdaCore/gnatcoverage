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
with Ada.Unchecked_Deallocation;
with Traces; use Traces;
with Traces_Dbase; use Traces_Dbase;
with Elf_Common; use Elf_Common;
with Elf_Arch; use Elf_Arch;
with Interfaces;
with Ada.Containers.Ordered_Sets;
with Strings; use Strings;
with System; use System;
with Elf_Files;
with Disa_Symbolize; use Disa_Symbolize;

package Traces_Elf is
   type Binary_Content is array (Elf_Arch.Elf_Size range <>)
     of Interfaces.Unsigned_8;

   type Binary_Content_Acc is access Binary_Content;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Binary_Content, Binary_Content_Acc);

   type Exe_File_Type is limited new Symbolizer with private;

   --  Open an ELF file.
   --  TEXT_START is the offset of .text section.
   --  Exception Elf_Files.Error is raised in case of error.
   procedure Open_File
     (Exec : out Exe_File_Type; Filename : String; Text_Start : Pc_Type);

   --  Build sections map for the current ELF file.
   procedure Build_Sections (Exec : in out Exe_File_Type);

   --  Show coverage of sections.
   procedure Disp_Sections_Coverage (Exec : Exe_File_Type; Base : Traces_Base);

   --  Fill Traces_Names with traces from BASE.
   procedure Add_Subprograms_Traces (Exec : Exe_File_Type; Base : Traces_Base);

   --  Using the executable, correctly set the state of every traces.
   procedure Set_Trace_State (Exec : Exe_File_Type;
                              Base : in out Traces_Base);
   procedure Set_Trace_State (Exec : Exe_File_Type;
                              Base : in out Traces_Base;
                              Section : Binary_Content);

   --  Read dwarfs info to build compile_units/subprograms lists.
   procedure Build_Debug_Compile_Units (Exec : in out Exe_File_Type);

   --  Read ELF symbol table.
   procedure Build_Symbols (Exec : in out Exe_File_Type);

   --  Read dwarfs info to build lines list.
   procedure Build_Debug_Lines (Exec : in out Exe_File_Type);

   --  Create per file line state.
   --  Also update lines state from traces state.
   procedure Build_Source_Lines (Exec : Exe_File_Type;
                                 Base : in out Traces_Base);

   procedure Build_Routine_Names (Exec : Exe_File_Type);

   --  Display lists.
   procedure Disp_Sections_Addresses (Exe : Exe_File_Type);
   procedure Disp_Compile_Units_Addresses (Exe : Exe_File_Type);
   procedure Disp_Subprograms_Addresses (Exe : Exe_File_Type);
   procedure Disp_Symbols_Addresses (Exe : Exe_File_Type);
   procedure Disp_Lines_Addresses (Exe : Exe_File_Type);

   type Addresses_Info;
   type Addresses_Info_Acc is access Addresses_Info;

   --  Display El.
   --  Mostly a debug procedure.
   procedure Disp_Address (El : Addresses_Info_Acc);

   --  Get symbol (if any) containing PC.
   function Get_Symbol (Exec : Exe_File_Type; Pc : Pc_Type)
                       return Addresses_Info_Acc;

   function "<" (L, R : Addresses_Info_Acc) return Boolean;

   package Addresses_Containers is new Ada.Containers.Ordered_Sets
     (Element_Type => Addresses_Info_Acc);

   type Addresses_Kind is
     (
      Section_Addresses,
      Compile_Unit_Addresses,
      Subprogram_Addresses,
      Symbol_Addresses,
      Line_Addresses
      );

   type Addresses_Info (Kind : Addresses_Kind := Section_Addresses) is record
      First, Last : Traces.Pc_Type;
      Parent : Addresses_Info_Acc;
      case Kind is
         when Section_Addresses =>
            Section_Name : String_Acc;
            Section_Index : Elf_Common.Elf_Half;
            Section_Content : Binary_Content_Acc;
         when Compile_Unit_Addresses =>
            Compile_Unit_Filename : String_Acc;
            Stmt_List : Interfaces.Unsigned_32;
         when Subprogram_Addresses =>
            Subprogram_Name : String_Acc;
         when Symbol_Addresses =>
            Symbol_Name : String_Acc;
         when Line_Addresses =>
            Line_Filename : String_Acc;
            Line_Number : Natural;
            Line_Next : Addresses_Info_Acc;
      end case;
   end record;

private
   type Exe_File_Type is limited new Symbolizer with record
      --  Sections index.
      Sec_Debug_Abbrev   : Elf_Half := 0;
      Sec_Debug_Info     : Elf_Half := 0;
      Sec_Debug_Info_Rel : Elf_Half := 0;
      Sec_Debug_Line     : Elf_Half := 0;
      Sec_Debug_Line_Rel : Elf_Half := 0;
      Sec_Debug_Str      : Elf_Half := 0;

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

      Sections_Set : Addresses_Containers.Set;
      Compile_Units_Set : Addresses_Containers.Set;
      Subprograms_Set : Addresses_Containers.Set;
      Symbols_Set : Addresses_Containers.Set;
      Lines_Set : Addresses_Containers.Set;
   end record;

   procedure Symbolize (Sym : Exe_File_Type;
                        Pc : Traces.Pc_Type;
                        Line : in out String;
                        Line_Pos : in out Natural);

end Traces_Elf;
