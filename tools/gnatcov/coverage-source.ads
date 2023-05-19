------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

with Checkpoints;             use Checkpoints;
with Files_Table;             use Files_Table;
with Instrument;              use Instrument;
with Instrument.Input_Traces; use Instrument.Input_Traces;
with Instrument.Base_Types;   use Instrument.Base_Types;
with Project;                 use Project;
with Traces;                  use Traces;
with Traces_Names;            use Traces_Names;
with Traces_Lines;            use Traces_Lines;
with SC_Obligations;          use SC_Obligations;

package Coverage.Source is

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

   procedure Compute_Source_Coverage
     (Subp_Key  : Subprogram_Key;
      Subp_Info : Subprogram_Info;
      T         : Trace_Entry);
   --  Analyze execution environment traces for the given subprogram to
   --  determine the coverage state of each SCO.

   procedure Compute_Source_Coverage
     (Filename             : String;
      Fingerprint          : SC_Obligations.Fingerprint_Type;
      CU_Name              : Compilation_Unit_Name;
      Bit_Maps_Fingerprint : SC_Obligations.Fingerprint_Type;
      Stmt_Buffer          : Coverage_Buffer;
      Decision_Buffer      : Coverage_Buffer;
      MCDC_Buffer          : Coverage_Buffer);
   --  Analyze source instrumentation traces to determine the coverage state
   --  of each SCO. Generic actual for
   --  Instrument.Input_Traces.Generic_Read_Source_Trace_File.

   procedure Compute_Line_State
     (Line_Num  : Positive;
      Line_Info : Line_Info_Access);
   --  Set Line.State based on coverage information accumulated on all SCOs
   --  that cover the given line.

   subtype SCO_State is Line_State range Not_Covered .. Undetermined_Coverage;
   function Get_Line_State
     (SCO   : SCO_Id;
      Level : Coverage_Level) return SCO_State;
   --  Return SCO's contribution to the state of the enclosing line, i.e.
   --  SCO's specific coverage state, ignoring any exemptions. This coverage
   --  is cumulative over all SCIs for this SCO, for the case of a SCO that
   --  has multiple tags (i.e. multiple, distinct coverage analyses).

   function Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag) return Boolean;
   --  True if any SCO in basic block has associated object code with then
   --  given tag.

   procedure Set_Basic_Block_Has_Code (SCO : SCO_Id; Tag : SC_Tag);
   --  Set Basic_Block_Has_Code for SCO (a Statement SCO) as well as all
   --  previous SCOs in its basic block, for the given tag.

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

   procedure Add_Unit (Unit : Project_Unit);
   --  Add Unit to the list of units of interest. For convenience, do nothing
   --  if it is invalid.

   procedure Compute_Unit_Name_For_Ignored_Sources;
   --  Compute the name of the owning unit for each known source file that is
   --  (sometimes or always) ignored.

   procedure Fill_Ignored_SF_Map;
   --  Iterate over all ignored source files to create a map indexed by
   --  unit names, linking them to the list of ignored source files.

   procedure Iterate_On_Unit_List
     (Process_Unit        : not null access procedure (Name : Project_Unit);
      Process_Source_File : not null access procedure (FI : File_Info))
   with Pre => Unit_List_Is_Valid;
   --  Call Unit_Callback for each unit of interest, passing to it the name of
   --  the unit, and call File_Callback for each (sometimes or always) ignored
   --  source file in the unit.
   --  Unit_Callback is called before iterating on the ignored files for that
   --  unit.

   -----------------
   -- Checkpoints --
   -----------------

   procedure Checkpoint_Save (CSS : access Checkpoint_Save_State);
   --  Save the current coverage state to S

   procedure Checkpoint_Clear;
   --  Clear the internal data structures used to create checkpoints

   procedure Checkpoint_Load (CLS : access Checkpoint_Load_State);
   --  Load checkpointed coverage state from S and merge into current state

end Coverage.Source;
