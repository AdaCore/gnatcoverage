------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

limited with LLVM_JSON_Checkpoints;
with Checkpoints;             use Checkpoints;
with Files_Table;             use Files_Table;
with Instrument;              use Instrument;
with Instrument.Input_Traces; use Instrument.Input_Traces;
with Logging;
with MC_DC;                   use MC_DC;
with SC_Obligations;          use SC_Obligations;
with Traces;                  use Traces;
with Traces_Names;            use Traces_Names;
with Traces_Lines;            use Traces_Lines;

package Coverage.Source is

   package Evaluation_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Evaluation);

   package Evaluation_Sets is new Ada.Containers.Ordered_Sets (Evaluation);

   Ignore_Exemptions_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("IGNORE_EXEMPTIONS");
   --  Note that enabling this trace will make gnatcov ignore source coverage
   --  exemptions (exemption pragmas will have no effect).

   procedure Initialize_SCI;
   --  Initialize source coverage information vector once SCOs have been loaded
   --  This can be called multiple times if SCO information is loaded in chunks
   --  (case of loading checkpointed coverage information).

   procedure Initialize_SCI_For_Instrumented_CU (CU : CU_Id);
   --  Initialize source coverage information vectors for obligations in CU,
   --  assumed to come from source instrumentation.
   --
   --  A particularity here is that we automatically consider CU's statements
   --  which have an allocated bit in trace buffers (see
   --  SC_Obligations.Statement_Bit_Map) as having code.

   function Decision_Requires_Assertion_Coverage (SCO : SCO_Id) return Boolean;
   --  True if SCO is for a decision in a pragma/aspect whose coverage can be
   --  checked by assertion coverage levels, and if any assertion coverage
   --  level is enabled.

   procedure Compute_Source_Coverage
     (Subp_Key  : Subprogram_Key;
      Subp_Info : Subprogram_Info;
      T         : Trace_Entry);
   --  Analyze execution environment traces for the given subprogram to
   --  determine the coverage state of each SCO.

   procedure Compute_Source_Coverage
     (Filename                : String;
      Fingerprint             : SC_Obligations.Fingerprint_Type;
      CU_Name                 : Compilation_Unit_Part;
      Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
      Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
      Stmt_Buffer             : Coverage_Buffer;
      Decision_Buffer         : Coverage_Buffer;
      MCDC_Buffer             : Coverage_Buffer);
   --  Analyze source instrumentation traces to determine the coverage state
   --  of each SCO. Generic actual for
   --  Instrument.Input_Traces.Generic_Read_Source_Trace_File.

   procedure Compute_Line_State
     (Line_Num  : Positive;
      Line_Info : Line_Info_Access;
      ST        : in out Scope_Traversal_Type);
   --  Set Line.State based on coverage information accumulated on all SCOs
   --  that cover the given line.
   --
   --  ST is used to filter the SCOs not belonging to subprograms of interest,
   --  it must be initialized to the CU for which we are computing coverage or
   --  left uninitialized.

   procedure Refine_Source_Coverage;
   --  Refine source coverage according to dominance information

   subtype SCO_State is Line_State range Not_Covered .. Undetermined_Coverage;
   function Get_Line_State
     (SCO   : SCO_Id;
      Level : Coverage_Level) return SCO_State;
   --  Return SCO's contribution to the state of the enclosing line, i.e.
   --  SCO's specific coverage state, ignoring any exemptions.

   function Basic_Block_Has_Code (SCO : SCO_Id) return Boolean;
   --  True if any SCO in basic block has associated object code

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id);
   --  Set Basic_Block_Has_Code for SCO (a Statement SCO) as well as all
   --  previous SCOs in its basic block.

   ---------------------------------------------------------
   -- Handling of the list of names for units of interest --
   ---------------------------------------------------------

   --  The two procedures below allow to manage a list of names for units of
   --  interest, which the --dump-units-to command-line option will dump.

   function Unit_List_Is_Valid return Boolean;
   --  Return whether the list of names for units of interest is valid. As soon
   --  as users explicitly pass files that contain SCOs/SID (though
   --  --scos/--sid, as opposed to selecting units of interest through our
   --  project file facilities), the list isn't valid and we are not able to
   --  dump it.

   procedure Invalidate_Unit_List (Reason : String)
      with Post => not Unit_List_Is_Valid;
   --  Set the list of names for units of interest as invalid. The Reason
   --  message is used for logging purposes, so that users can find out why we
   --  cannot dump the list of units of interest.

   procedure Add_Unit (Unit : Compilation_Unit);
   --  Add Unit to the list of units of interest. For convenience, do nothing
   --  if it is invalid.

   procedure Compute_Unit_Name_For_Ignored_Sources;
   --  Compute the name of the owning unit for each known source file that is
   --  (sometimes or always) ignored.

   procedure Fill_Ignored_SF_Map;
   --  Iterate over all ignored source files to create a map indexed by
   --  unit names, linking them to the list of ignored source files.

   procedure Iterate_On_Unit_List
     (Process_Unit        : not null access procedure
        (Name : Compilation_Unit);
      Process_Source_File : not null access procedure (FI : File_Info))
   with Pre => Unit_List_Is_Valid;
   --  Call Process_Unit for each unit of interest, passing to it the name of
   --  the unit, and call Process_Source_File for each (sometimes or always)
   --  ignored source file in the unit.
   --
   --  Process_Unit is called before iterating on the ignored files for that
   --  unit.

   procedure Report_Units (File : File_Type);
   --  Print a report about units of interest as well as ignored source files
   --  to File.

   -----------------
   -- Checkpoints --
   -----------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State);
   --  Save the current coverage state to S

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : in out Checkpoint_Load_State);
   --  Load checkpointed coverage state from S and merge into current state

   procedure LLVM_JSON_Load
     (Ckpt : access constant LLVM_JSON_Checkpoints.LLVM_Coverage_Ckpt);
   --  Register statement counters and MC/DC evaluations from Report.

end Coverage.Source;
