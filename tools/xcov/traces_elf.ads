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
with Elf_Common;
with Elf_Arch;
with Interfaces;
with Ada.Containers.Ordered_Sets;

package Traces_Elf is
   type String_Acc is access String;

   --  Open an ELF file.
   --  TEXT_START is the offset of .text section.
   --  Exception Elf_Files.Error is raised in case of error.
   procedure Open_File (Filename : String; Text_Start : Pc_Type);

   --  Build sections map for the current ELF file.
   procedure Build_Sections;

   --  Show coverage of sections.
   procedure Disp_Sections_Coverage;

   --  Show coverage of subprograms.
   procedure Disp_Subprograms_Coverage;

   --  Using the executable, correctly set the state of every traces.
   procedure Set_Trace_State;

   --  Read dwarfs info to build compile_units/subprograms lists.
   procedure Build_Debug_Compile_Units;

   --  Read ELF symbol table.
   procedure Build_Symbols;

   --  Read dwarfs info to build lines list.
   procedure Build_Debug_Lines;

   --  Create per file line state.
   --  Also update lines state from traces state.
   procedure Build_Source_Lines;

   --  Display lists.
   procedure Disp_Sections_Addresses;
   procedure Disp_Compile_Units_Addresses;
   procedure Disp_Subprograms_Addresses;
   procedure Disp_Symbols_Addresses;
   procedure Disp_Lines_Addresses;

   type Addresses_Info;
   type Addresses_Info_Acc is access Addresses_Info;

   --  Display El.
   --  Mostly a debug procedure.
   procedure Disp_Address (El : Addresses_Info_Acc);

   type Addresses_Line_Chain is private;
   procedure Append (Chain : in out Addresses_Line_Chain;
                     Line : Addresses_Info_Acc);
   function Get_First (Chain : Addresses_Line_Chain)
                      return Addresses_Info_Acc;
   function Get_Line_Next (Line : Addresses_Info_Acc)
                          return Addresses_Info_Acc;

   type Binary_Content is array (Elf_Arch.Elf_Size range <>)
     of Interfaces.Unsigned_8;

   type Binary_Content_Acc is access Binary_Content;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Binary_Content, Binary_Content_Acc);


   --  Return the symbol for Addr followed by a colon (':').
   --  Return an empty string if none.
   function Get_Label (Info : Addresses_Info_Acc) return String;

   --  Generate the disassembly for INSN.
   --  INSN is exactly one instruction.
   --  PC is the target address of INSN (used to display branch targets).
   function Disassemble (Insn : Binary_Content; Pc : Pc_Type) return String;

   --  Call CB for each insn in INFO.
   procedure Disp_Assembly_Lines
     (Info : Addresses_Info_Acc;
      Cb : access procedure (Addr : Pc_Type;
                             State : Trace_State;
                             Insn : Binary_Content));

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
   type Addresses_Line_Chain is record
      First, Last : Addresses_Info_Acc := null;
   end record;
end Traces_Elf;
