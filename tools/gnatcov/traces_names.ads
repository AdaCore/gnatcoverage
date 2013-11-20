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

--  Management of the routines database

with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GNATCOLL.Utils;   use GNATCOLL.Utils;

with Coverage;      use Coverage;
with Traces;        use Traces;
with Traces_Dbase;  use Traces_Dbase;
with Traces_Lines;  use Traces_Lines;
with Traces_Elf;    use Traces_Elf;

package Traces_Names is

   type Routines_Of_Interest_Origin_Type is
     (Invalid_Origin, From_Command_Line, From_Elf_Symbols);

   Routines_Of_Interest_Origin : Routines_Of_Interest_Origin_Type :=
     Invalid_Origin;
   --  Specify where the list of routines of interest comes from. Set right
   --  after arguments parsing in GNATcov.

   procedure Add_Routine_Of_Interest (Name : String);
   --  Add a routine name that will be suject to coverage

   function Is_Routine_Of_Interest (Name : String) return Boolean;
   --  Return if a routine name will be suject to coverage

   procedure Remove_Routine_Of_Interest (Name : String);
   --  Remove a routine name so it will not be suject to coverage

   type Subprogram_Key is record
      Name         : Symbol;
      Compile_Unit : Symbol;
      --  Identify a subprogram in a unique way, even for homonymous symbols.
      --  If there is no debug information, Compile_Unit must be null.

      Origin       : Natural := 0;
      --  If Compile_Unit is null, this member make a difference between
      --  similar homonym symbols. Setting this member is done when calling
      --  Add_Routine. The caller should preserve its value somewhere in order
      --  to get back the added routine from the Routines map.
   end record;

   --  Information recorded about each subprogram in the routines database

   type Subprogram_Info is record
      Exec  : Exe_File_Acc;
      --  Pointer to the Exec file where this subprogram has first been found

      Insns : Binary_Content_Acc;
      --  Subprogram binary content

      Traces : Traces_Base_Acc;
      --  Traces for the subprogram

      Offset : Pc_Type := No_PC;
      --  Offset to be added to trace PCs to rebase them for the reference
      --  code in Insns. Updated each time Add_Code is used to register a new
      --  instance of the code for this routine.

      Routine_Tag : SC_Tag := No_SC_Tag;
      --  Routine tag used when doing source coverage analysis with per-routine
      --  tagging.
   end record;

   function Key_To_Name (Key : Subprogram_Key) return Cst_String_Access;
   --  Return the symbol name associated to a key. The called must not be
   --  deallocated.

   procedure Add_Routine
     (Key  : in out Subprogram_Key;
      Exec : Exe_File_Acc;
      Tag  : out SC_Tag);
   --  Create a Subprogram_Info entry for the given Subprogram_Key in the
   --  routines database if it doesn't exist. Key.Origin member is updated if
   --  there is no associated Compile_Unit. Returns the assigned routine tag.

   function Is_In (Key : Subprogram_Key) return Boolean;
   --  Return True iff Key has been included into the routine database

   function Get_Subp_Info (Key : Subprogram_Key) return Subprogram_Info;
   --  Return subprogram info for some routine.

   procedure Key_From_Symbol
     (Exec : Exe_File_Acc;
      Sym  : Address_Info_Acc;
      Key  : out Subprogram_Key);
   --  Format a "Key" for the "Sym" symbol in the "Exec" binary file.

   procedure Iterate
     (Proc : access procedure (Subp_Key  : Subprogram_Key;
                               Subp_Info : in out Subprogram_Info));
   --  Execute Proc for each routine in the database

   procedure Read_Routine_Names_From_Text (Filename : String);
   --  Read a list of routines name from a text file in the following format:
   --  * lines starting with '#' are ignored
   --  * one name per line
   --  * no blanks allowed.

   procedure Disp_All_Routines_Of_Interest;
   --  Display the list of routines that will be suject to coverage (on
   --  standard output).

   procedure Disp_All_Routines;
   --  Display the list of routines (on standard output).

   procedure Add_Code
     (Subp_Key     : Subprogram_Key;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content;
      First_Code   : out Boolean;
      Subp_Info    : out Subprogram_Info);
   --  Add code for Subp_Key from Exec with the given contents. Updates the
   --  offset to be added to traces relative to Exec for this routine to rebase
   --  them for the recorded code chunk stored in the routines database.  If
   --  this was the first time code was seen for this routine, First_Code is
   --  set true. The entry from the routine names table (Subp_Info) is
   --  returned.

   procedure Add_Code_And_Traces
     (Subp_Key     : Subprogram_Key;
      Exec         : Exe_File_Acc;
      Content      : Binary_Content;
      Base         : access Traces_Base);
   --  Add code for the Subp_Key routine to its record
   --
   --  Optionally also add a set of execution traces (if Base is not null)
   --
   --  Parameters:
   --  * Subp_Key: key of the routine to consider;
   --  * Exec: handle to the executable that generated the execution traces
   --  that we consider.
   --  * Content: slice of the binary content of Exec's .text section that
   --    corresponds to the routine to consider (Content'First being the
   --    address of the routine in Exec's .text);
   --  * Base: execution traces to merge into the routine's trace database.
   --
   --  As the execution traces shall have been generated by the execution of
   --  Exec, the traces that corresponds to the routine to consider should
   --  have their execution addresses (Last, First) in a non-empty
   --  intersection with Content'Range. Or, in order words, such an entry E
   --  should verify:
   --
   --     E.First in Content'Range or E.Last in Content'Range
   --
   --  Any trace that does not verify the condition will be dropped. At the
   --  contrary, a trace that verifies this condition will be added to the
   --  corresponding subprogram traces, provided that the routine's content
   --  in Exec is the same as the one of the corresponding subprogram in the
   --  routine database (if this condition is not met, a Consolidation_Error
   --  is raised). The trace may also be rebased and split before being added
   --  to the routine traces, to verify:
   --
   --     E.First in Subp_Info.Insns'Range and E.Last in Subp_Info.Insns'Range
   --
   --  (Subp_Info being the corresponding subprogram info in the routine
   --  database).

   function Compute_Routine_State
     (Insns  : Binary_Content_Acc;
      Traces : Traces_Base_Acc) return Line_State;
   --  Compute routine state from its object coverage information and from its
   --  content.

   Consolidation_Error : exception;
   --  Raised if consolidation is not possible (eg different code for a
   --  function).
end Traces_Names;
