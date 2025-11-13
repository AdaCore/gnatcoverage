------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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
--                                                                          --
--  LLVM coverage report interface. Relies on the LLVM_JSON trace adapter.  --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;   use GNATCOLL.JSON;
with GNATCOLL.Traces; use GNATCOLL.Traces;

with Files_Table;
with JSON;
with MC_DC;
with Outputs; use Outputs;

--  The exporter tool dumps JSON files that have the following architecture:
--  {
--     "data": [
--        {
--           "filename": str,
--           "functions": [
--              {
--                 "name": str,
--                 "demangled_name": str,
--                 "code_regions": [
--                    {
--                       "count": int,
--                       "kind": str,
--                       "span": [start_line, start_col, end_line, end_col]
--                    }*
--                 ],
--                 "branch_regions": [
--                    {
--                       "true_count": int,
--                       "false_count": int,
--                       "kind": str,
--                       "span": [start_line, start_col, end_line, end_col]
--                    }*
--                 ],
--                 "mcdc_records": [
--                    {
--                       "span": [start_line, start_col, end_line, end_col],
--                       "num_conditions": int,
--                       "test_vectors": [
--                          {
--                             "result": (0, 1) for False or True,
--                             "vector":
--                                list of (-1, 0, 1) for DontCare, False, True
--                          }*
--                       ]
--                    }*
--                 ]
--              }*
--           ]
--        }*
--     ]
--  }

package body LLVM_JSON_Checkpoints is

   type Parsed_MCDC_Record is record
      Span           : Local_Source_Location_Range;
      Num_Conditions : Natural;
      Test_Vectors   : Evaluation_Vectors.Vector;
   end record;

   type Parsed_Branch_Region is record
      Span         : Local_Source_Location_Range;
      T_Exec_Count : Natural;
      F_Exec_Count : Natural;
   end record;

   type Parsed_Code_Region is record
      Span            : Local_Source_Location_Range;
      Execution_Count : Natural;
   end record;

   --------------------------
   -- Forward declarations --
   --------------------------

   function Count_Cond_For_Dec
     (Regions : LLVM_Region_Vector.Vector; Dec_Id : Valid_LLVM_Region_Id)
      return Natural;
   --  Helper function for the Post-Condition of Build_Ckpt_From_JSON.

   function Build_Ckpt_From_JSON
     (JSON_Filename : String) return LLVM_Coverage_Ckpt;

   procedure Print_Report (Ckpt : LLVM_Coverage_Ckpt);
   --  Print the Report on STDOUT.
   --  This is a debug helper function only.

   procedure Add_MCDC_To_Regions
     (MCDC_Record : Parsed_MCDC_Record;
      Regions     : in out LLVM_Region_Vector.Vector);

   procedure Add_Branch_Region_To_Regions
     (Branch_Region : Parsed_Branch_Region;
      Regions       : in out LLVM_Region_Vector.Vector);

   procedure Add_Code_Region_To_Regions
     (Code_Region : Parsed_Code_Region;
      Regions     : in out LLVM_Region_Vector.Vector);

   function Find_Parent_Decision
     (Regions : LLVM_Region_Vector.Vector;
      Span    : Local_Source_Location_Range;
      Id      : out LLVM_Region_Id) return Boolean;
   --  Search for an already existing region in Regions which
   --  has the Decision kind and which Span includes the given span.
   --  Return True if such element is found, and sets Id accordingly.

   function Parse_MCDC_Record
     (JSON_MCDC_Record : JSON_Value) return Parsed_MCDC_Record;

   function Parse_Branch_Region
     (JSON_Branch_Region : JSON_Value) return Parsed_Branch_Region;

   function Parse_Code_Region
     (JSON_Code_Region : JSON_Value) return Parsed_Code_Region;

   function Parse_Test_Vector
     (JSON_Test_Vector : JSON_Value) return MC_DC.Evaluation;

   function Parse_SLOC_Range
     (Sloc_Array : JSON_Array) return Local_Source_Location_Range;
   --  Given a JSON_Value, checks that the value is a 4 long array of integers
   --  [a b c d] and return a Local_Source_Location_Range ((a, b), (c, d))

   ---------------
   -- JSON_Load --
   ---------------

   procedure JSON_Load (JSON_Filename : String) is
      Ckpt : aliased LLVM_Coverage_Ckpt :=
        Build_Ckpt_From_JSON (JSON_Filename);
   begin
      LLVM_Trace.Trace ("Report generated");
      if LLVM_Trace.Is_Active then
         Print_Report (Ckpt);
      end if;

      Files_Table.LLVM_JSON_Load (Ckpt'Access);
      SC_Obligations.LLVM_JSON_Load (Ckpt'Access);
      Coverage.Source.LLVM_JSON_Load (Ckpt'Access);
   end JSON_Load;

   --------------------------
   -- Build_Ckpt_From_JSON --
   --------------------------

   function Build_Ckpt_From_JSON
     (JSON_Filename : String) return LLVM_Coverage_Ckpt
   is
      JSON_Files : constant JSON_Array :=
        JSON.Child_Array (JSON.Read_File (JSON_Filename), "data");

      Report : LLVM_Coverage_Ckpt :=
        (JSON_Filename => +JSON_Filename, others => <>);
   begin
      for JSON_File of JSON_Files loop
         declare
            Functions : constant JSON_Array :=
              JSON.Child_Array (JSON_File, "functions");

            File_Report : LLVM_Coverage_File_Ckpt :=
              (Filename => +JSON.Child_String (JSON_File, "filename"),
               others   => <>);
         begin
            for Fct of Functions loop
               declare
                  MCDC_Records   : constant JSON_Array :=
                    JSON.Child_Array (Fct, "mcdc_records");
                  Code_Regions   : constant JSON_Array :=
                    JSON.Child_Array (Fct, "code_regions");
                  Branch_Regions : constant JSON_Array :=
                    JSON.Child_Array (Fct, "branch_regions");

                  Function_Report : LLVM_Coverage_Function_Ckpt :=
                    (Name         =>
                       +JSON.Child_String (Fct, "demangled_name"),
                     Mangled_Name => +JSON.Child_String (Fct, "name"),
                     others       => <>);
               begin
                  --  Load MCDC records
                  --  It is important to load MCDC records BEFORE code regions.
                  --  That way, decision regions (and thus decision SCOs
                  --  later on) are inserted in the vector BEFORE their
                  --  conditions which are populated when loading code regions.
                  --
                  --  We rely on this assumption to more efficiently build
                  --  the SCO descriptors.

                  LLVM_Trace.Trace
                    ("Build_Ckpt_From_JSON: handle function '"
                     & (+Function_Report.Name)
                     & "'");

                  for MCDC_Record of MCDC_Records loop
                     Add_MCDC_To_Regions
                       (Parse_MCDC_Record (MCDC_Record),
                        Function_Report.Regions);
                  end loop;

                  --  Complete decision regions with branch regions.
                  --  Branches may be conditions of an existing decision.
                  --  (1-condition decisions are not registered through MCDC)

                  for Branch_Region of Branch_Regions loop
                     Add_Branch_Region_To_Regions
                       (Parse_Branch_Region (Branch_Region),
                        Function_Report.Regions);
                  end loop;

                  --  Load code regions

                  for Code_Region of Code_Regions loop
                     Add_Code_Region_To_Regions
                       (Parse_Code_Region (Code_Region),
                        Function_Report.Regions);
                  end loop;

                  File_Report.Functions.Append (Function_Report);
               end;
            end loop;

            Report.File_Reports.Append (File_Report);
         end;
      end loop;

      --  Ensure the Report is well-formed (check that we found all the
      --  conditions from each decision).

      for File of Report.File_Reports loop
         for Fct of File.Functions loop
            for Reg of Fct.Regions loop
               if Reg.Kind = Decision then
                  if Reg.Num_Conditions = 0 then
                     Fatal_Error
                       ("Invalid JSON: Decision with 0 condition "
                        & "found in function: "
                        & (+Fct.Name));
                  end if;
                  declare
                     Count : constant Natural :=
                       Count_Cond_For_Dec (Fct.Regions, Reg.Id);
                  begin
                     if Reg.Num_Conditions /= Count then
                        Fatal_Error
                          ("Invalid JSON: Decision declares "
                           & Natural'Image (Reg.Num_Conditions)
                           & " conditions, but "
                           & Natural'Image (Count)
                           & " found in function: "
                           & (+Fct.Name));
                     end if;
                  end;
               end if;
            end loop;
         end loop;
      end loop;

      return Report;
   end Build_Ckpt_From_JSON;

   ------------------------
   -- Count_Cond_For_Dec --
   ------------------------

   function Count_Cond_For_Dec
     (Regions : LLVM_Region_Vector.Vector; Dec_Id : Valid_LLVM_Region_Id)
      return Natural
   is
      Count : Natural := 0;
   begin
      for Region of Regions loop
         if Region.Kind = Condition and then Region.Parent_Id = Dec_Id then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Cond_For_Dec;

   ------------------
   -- Print_Report --
   ------------------

   procedure Print_Report (Ckpt : LLVM_Coverage_Ckpt) is
   begin
      for File of Ckpt.File_Reports loop
         LLVM_Trace.Trace ("file: " & (+File.Filename));
         LLVM_Trace.Increase_Indent;
         for Fct of File.Functions loop
            LLVM_Trace.Trace
              ("function: "
               & (+Fct.Name)
               & " (mangled: "
               & (+Fct.Mangled_Name)
               & ")");
            LLVM_Trace.Increase_Indent;
            for Region of Fct.Regions loop
               case Region.Kind is
                  when Statement =>
                     null;
                     LLVM_Trace.Trace
                       ("["
                        & LLVM_Region_Id'Image (Region.Id)
                        & "] STATEMENT "
                        & Image (Region.Span)
                        & ":");
                     LLVM_Trace.Trace
                       ("     Count: "
                        & Natural'Image (Region.Execution_Count));

                  when Decision  =>
                     null;
                     LLVM_Trace.Trace
                       ("["
                        & LLVM_Region_Id'Image (Region.Id)
                        & "] DECISION  "
                        & Image (Region.Span)
                        & ":");

                     for TV of Region.Test_Vectors loop
                        declare
                           Buffer : Strings.US.Unbounded_String := +"     { ";
                        begin
                           for Eval of TV.Values loop
                              Append
                                (Buffer,
                                 (case Eval is
                                    when Unknown => "-, ",
                                    when False   => "F, ",
                                    when True    => "T, "));
                           end loop;
                           Append (Buffer, "} -> ");
                           Append
                             (Buffer,
                              (case TV.Outcome is
                                 when Unknown => "X",
                                 when False   => "F",
                                 when True    => "T"));

                           LLVM_Trace.Trace (+Buffer);
                        end;
                     end loop;

                  when Condition =>
                     null;
                     LLVM_Trace.Trace
                       ("["
                        & LLVM_Region_Id'Image (Region.Id)
                        & "] CONDITION "
                        & Image (Region.Span)
                        & ": parent "
                        & LLVM_Region_Id'Image (Region.Parent_Id));
               end case;
            end loop;
            LLVM_Trace.Decrease_Indent;
         end loop;
         LLVM_Trace.Decrease_Indent;
      end loop;
   end Print_Report;

   -------------------------
   -- Add_MCDC_To_Regions --
   -------------------------

   procedure Add_MCDC_To_Regions
     (MCDC_Record : Parsed_MCDC_Record;
      Regions     : in out LLVM_Region_Vector.Vector)
   is
      Region : constant LLVM_Region :=
        (Kind           => Decision,
         Span           => MCDC_Record.Span,
         Id             => Regions.Last_Index + 1,
         Num_Conditions => MCDC_Record.Num_Conditions,
         Test_Vectors   => MCDC_Record.Test_Vectors,
         others         => <>);
   begin
      LLVM_Trace.Trace ("Add_MCDC_Region: " & Image (MCDC_Record.Span));

      Regions.Append (Region);
   end Add_MCDC_To_Regions;

   ----------------------------------
   -- Add_Branch_Region_To_Regions --
   ----------------------------------

   procedure Add_Branch_Region_To_Regions
     (Branch_Region : Parsed_Branch_Region;
      Regions       : in out LLVM_Region_Vector.Vector)
   is
      Cond_Region : LLVM_Region;
      Dec_Region  : LLVM_Region;
      Parent_Id   : LLVM_Region_Id;
   begin
      --  Branch regions are used to find 1-condition decisions, which are not
      --  instrumented for MCDC.
      --  If a Decision already exist, drop the branch.
      --  Otherwise, build a decision AND a condition Regions corresponding to
      --  the decision.

      LLVM_Trace.Trace ("Add_Branch_Region " & Image (Branch_Region.Span));

      if Find_Parent_Decision (Regions, Branch_Region.Span, Parent_Id) then
         pragma Assert (Parent_Id /= No_LLVM_Region_Id);
         LLVM_Trace.Trace
           ("Add_Branch_Region: Parent decision found. "
            & "Adding as a condition");
         declare
            Parent_Region : LLVM_Region renames Regions.Reference (Parent_Id);
         begin
            Cond_Region :=
              (Kind      => Condition,
               Span      => Branch_Region.Span,
               Id        => Regions.Last_Index + 1,
               Parent_Id => Parent_Id,
               Index     => Parent_Region.Next_Condition,
               others    => <>);
            Parent_Region.Next_Condition := Parent_Region.Next_Condition + 1;
         end;
         Regions.Append (Cond_Region);
      else
         declare
            TVs : Evaluation_Vectors.Vector;
         begin
            --  Build True and False Test vectors.
            --  We only keep track of 1 execution, because we are not
            --  interested in knowing how many times these were executed.

            LLVM_Trace.Trace
              ("Add_Branch_Region: No parent decision. "
               & "Create a 1-condition decision.");

            if Branch_Region.T_Exec_Count > 0 then
               declare
                  Cond_Vec : Condition_Evaluation_Vectors.Vector;
               begin
                  Cond_Vec.Append (True);
                  TVs.Append
                    ((Decision       => No_SCO_Id,
                      Values         => Cond_Vec,
                      Outcome        => True,
                      Next_Condition => No_Condition_Index));
               end;
            end if;
            if Branch_Region.F_Exec_Count > 0 then
               declare
                  Cond_Vec : Condition_Evaluation_Vectors.Vector;
               begin
                  Cond_Vec.Append (False);
                  TVs.Append
                    ((Decision       => No_SCO_Id,
                      Values         => Cond_Vec,
                      Outcome        => False,
                      Next_Condition => No_Condition_Index));
               end;
            end if;

            Dec_Region :=
              (Kind           => Decision,
               Span           => Branch_Region.Span,
               Id             => Regions.Last_Index + 1,
               Num_Conditions => 1,
               Test_Vectors   => TVs,
               others         => <>);
            Regions.Append (Dec_Region);

            Cond_Region :=
              (Kind      => Condition,
               Span      => Branch_Region.Span,
               Id        => Regions.Last_Index + 1,
               Parent_Id => Regions.Last_Index,
               --  Previous Decision.
               Index     => Condition_Index'First,
               others    => <>);
            Regions.Append (Cond_Region);
         end;
      end if;
   end Add_Branch_Region_To_Regions;

   --------------------------------
   -- Add_Code_Region_To_Regions --
   --------------------------------

   procedure Add_Code_Region_To_Regions
     (Code_Region : Parsed_Code_Region;
      Regions     : in out LLVM_Region_Vector.Vector)
   is
      Region    : LLVM_Region;
      Parent_Id : LLVM_Region_Id;
   begin
      LLVM_Trace.Trace ("Add_Code_Region: " & Image (Code_Region.Span));

      if not Find_Parent_Decision (Regions, Code_Region.Span, Parent_Id) then
         pragma Assert (Parent_Id = No_LLVM_Region_Id);
         LLVM_Trace.Trace
           ("Add_Code_Region: No parent decision found. "
            & "Adding as a Statement");

         Region :=
           (Kind            => Statement,
            Span            => Code_Region.Span,
            Id              => Regions.Last_Index + 1,
            Execution_Count => Code_Region.Execution_Count,
            others          => <>);
         Regions.Append (Region);
      end if;
   end Add_Code_Region_To_Regions;

   --------------------------
   -- Find_Parent_Decision --
   --------------------------

   function Find_Parent_Decision
     (Regions : LLVM_Region_Vector.Vector;
      Span    : Local_Source_Location_Range;
      Id      : out LLVM_Region_Id) return Boolean
   is
      Current_Best : LLVM_Region_Id := No_LLVM_Region_Id;
   begin
      for Index in Regions.First_Index .. Regions.Last_Index loop
         declare
            Region : constant LLVM_Region :=
              Regions.Constant_Reference (Index);
         begin
            if Region.Kind = Decision and then Contained_In (Span, Region.Span)
            then
               if Current_Best /= No_LLVM_Region_Id then

                  --  A matching decision region was already found.

                  if Contained_In (Region.Span, Regions (Current_Best).Span)
                  then
                     --  The new region is smaller.

                     Current_Best := Region.Id;
                  end if;
               else
                  --  It is the first matching region.

                  Current_Best := Region.Id;
               end if;
            end if;
         end;
      end loop;

      Id := Current_Best;
      if Current_Best = No_LLVM_Region_Id then
         return False;
      else
         return True;
      end if;
   end Find_Parent_Decision;

   -----------------------
   -- Parse_MCDC_Record --
   -----------------------

   function Parse_MCDC_Record
     (JSON_MCDC_Record : JSON_Value) return Parsed_MCDC_Record
   is
      JSON_Evaluation_Array : constant JSON_Array :=
        JSON.Child_Array (JSON_MCDC_Record, "test_vectors");

      Parsed : Parsed_MCDC_Record :=
        (Span           =>
           Parse_SLOC_Range (JSON.Child_Array (JSON_MCDC_Record, "span")),
         Num_Conditions => JSON.Child_Int (JSON_MCDC_Record, "num_conditions"),
         others         => <>);
   begin
      for JSON_Evaluation of JSON_Evaluation_Array loop
         Parsed.Test_Vectors.Append (Parse_Test_Vector (JSON_Evaluation));
      end loop;
      return Parsed;
   end Parse_MCDC_Record;

   -------------------------
   -- Parse_Branch_Region --
   -------------------------

   function Parse_Branch_Region
     (JSON_Branch_Region : JSON_Value) return Parsed_Branch_Region
   is
      Branch_Region : constant Parsed_Branch_Region :=
        (Span         =>
           Parse_SLOC_Range (JSON.Child_Array (JSON_Branch_Region, "span")),
         T_Exec_Count => JSON.Child_Int (JSON_Branch_Region, "true_count"),
         F_Exec_Count => JSON.Child_Int (JSON_Branch_Region, "false_count"));
   begin
      return Branch_Region;
   end Parse_Branch_Region;

   -----------------------
   -- Parse_Code_Region --
   -----------------------

   function Parse_Code_Region
     (JSON_Code_Region : JSON_Value) return Parsed_Code_Region
   is
      Code_Region : constant Parsed_Code_Region :=
        (Span            =>
           Parse_SLOC_Range (JSON.Child_Array (JSON_Code_Region, "span")),
         Execution_Count => JSON.Child_Int (JSON_Code_Region, "count"));
   begin
      return Code_Region;
   end Parse_Code_Region;

   -----------------------
   -- Parse_Test_Vector --
   -----------------------

   function Parse_Test_Vector
     (JSON_Test_Vector : JSON_Value) return MC_DC.Evaluation
   is
      Result_Int : constant Integer :=
        JSON.Child_Int (JSON_Test_Vector, "decision_outcome");
      Vector     : constant JSON_Array :=
        JSON.Child_Array (JSON_Test_Vector, "vector");
      Size       : constant Integer := Length (Vector);

      Outcome  : Tristate;
      Cond_Vec : Condition_Evaluation_Vectors.Vector;
      Result   : MC_DC.Evaluation;
   begin
      if Result_Int = 1 then
         Outcome := True;
      elsif Result_Int = 0 then
         Outcome := False;
      else
         Fatal_Error ("Expected 0 or 1, got " & Integer'Image (Result_Int));
      end if;

      for I in 1 .. Size loop
         declare
            Cond_Int : constant Integer := JSON.Array_Nth_Integer (Vector, I);
         begin
            if Cond_Int = -1 then
               Cond_Vec.Append (Unknown);
            elsif Cond_Int = 0 then
               Cond_Vec.Append (False);
            elsif Cond_Int = 1 then
               Cond_Vec.Append (True);
            else
               Fatal_Error
                 ("Expected one of (-1,0,1), got " & Integer'Image (Cond_Int));
            end if;
         end;
      end loop;

      Result.Decision := No_SCO_Id;
      Result.Values := Cond_Vec;
      Result.Outcome := Outcome;
      Result.Next_Condition := No_Condition_Index;
      return Result;
   end Parse_Test_Vector;

   ----------------------
   -- Parse_SLOC_Range --
   ----------------------

   function Parse_SLOC_Range
     (Sloc_Array : JSON_Array) return Local_Source_Location_Range is
   begin
      if Length (Sloc_Array) /= 4 then
         Fatal_Error ("'span' array should be 4 integers");
      end if;

      return
        ((JSON.Array_Nth_Integer (Sloc_Array, 1),
          JSON.Array_Nth_Integer (Sloc_Array, 2)),
         (JSON.Array_Nth_Integer (Sloc_Array, 3),
          JSON.Array_Nth_Integer (Sloc_Array, 4)));
   end Parse_SLOC_Range;

end LLVM_JSON_Checkpoints;
