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

--  Source trace files decoding

private with Ada.Containers.Ordered_Maps;
private with Ada.Unchecked_Deallocation;

with SC_Obligations;
with Traces_Files;
with Traces_Source;

package Instrument.Input_Traces is

   type Coverage_Buffer is array (Any_Bit_Id range <>) of Boolean with Pack;
   --  Content of a coverage buffer

   generic
      with
        procedure On_Trace_Info
          (Kind : Traces_Source.Supported_Info_Kind; Data : String) is <>;
      with
        procedure On_Trace_Entry
          (Filename                : String;
           Fingerprint             : SC_Obligations.Fingerprint_Type;
           CU_Name                 : Compilation_Unit_Part;
           Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
           Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
           Stmt_Buffer             : Coverage_Buffer;
           Decision_Buffer         : Coverage_Buffer;
           MCDC_Buffer             : Coverage_Buffer) is <>;
   procedure Generic_Read_Source_Trace_File
     (Filename : String; Result : out Traces_Files.Read_Result);
   --  Read the given Filename source trace file and call:
   --
   --    * On_Trace_Info on each decoded trace info entry;
   --    * On_Trace_Entry on each decoded trace entry.
   --
   --  If successful, Result.Success is set to True. Otherwise, Result is set
   --  to the corresponding error information.

   procedure Dump_Source_Trace_File (Filename : String);
   --  Read the given Filename source trace file and dump its content on the
   --  standard output. Emit a fatal error if the file is invalid.

   procedure Extract_Base64_Trace (Input_File, Output_File : String);
   --  Read the file at Input and extract Base64-encoded traces from it (see
   --  GNATcov_RTS.Traces.Generic_Output.Write_Trace_File_Base64). Then write a
   --  source trace file to the file at Output with our binary format.

   type Consolidation_State is private;
   --  Consolidated state of source traces. This is used to speed up the
   --  loading of source coverage traces: call ``Update_State`` to register all
   --  coverage buffers from all source traces (consolidation between source
   --  traces is very fast), then call ``Process_State`` to compute code
   --  coverage from the resulting consolidated source trace (this is slower,
   --  and thus done only once for all source traces rather than once per
   --  source trace).

   procedure Update_State
     (Self                    : in out Consolidation_State;
      Filename                : String;
      Fingerprint             : SC_Obligations.Fingerprint_Type;
      CU_Name                 : Compilation_Unit_Part;
      Bit_Maps_Fingerprint    : SC_Obligations.Fingerprint_Type;
      Annotations_Fingerprint : SC_Obligations.Fingerprint_Type;
      Stmt_Buffer             : Coverage_Buffer;
      Decision_Buffer         : Coverage_Buffer;
      MCDC_Buffer             : Coverage_Buffer);
   --  Update ``Self`` with data from the given coverage buffers

   procedure Process_State (Self : in out Consolidation_State);
   --  Update gnatcov's code coverage state with data from the coverage buffers
   --  added to ``Self``.

   procedure Release (Self : in out Consolidation_State);
   --  Release all resources associated to ``Self``

private

   type Consolidated_Trace_Key is record
      CU_Name     : Compilation_Unit_Part;
      Fingerprint : SC_Obligations.Fingerprint_Type;
   end record;

   function "<" (Left, Right : Consolidated_Trace_Key) return Boolean;

   type Consolidated_Trace_Entry_Record
     (Last_Stmt_Bit     : Any_Bit_Id;
      Last_Decision_Bit : Any_Bit_Id;
      Last_MCDC_Bit     : Any_Bit_Id)
   is record
      CU              : CU_Id;
      Stmt_Buffer     : Coverage_Buffer (0 .. Last_Stmt_Bit);
      Decision_Buffer : Coverage_Buffer (0 .. Last_Decision_Bit);
      MCDC_Buffer     : Coverage_Buffer (0 .. Last_MCDC_Bit);
   end record;
   type Consolidated_Trace_Entry is access all Consolidated_Trace_Entry_Record;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Consolidated_Trace_Entry_Record,
        Consolidated_Trace_Entry);

   package Consolidated_Trace_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Consolidated_Trace_Key,
        Element_Type => Consolidated_Trace_Entry);

   type Consolidation_State is record
      Map : Consolidated_Trace_Maps.Map;
   end record;

end Instrument.Input_Traces;
