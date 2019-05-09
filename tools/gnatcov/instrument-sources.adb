------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.VFS;

with Langkit_Support.Text;    use Langkit_Support.Text;
with Libadalang.Common;       use Libadalang.Common;

with SCOs;

with Coverage;        use Coverage;
with Files_Table;     use Files_Table;
with Instrument.Tree; use Instrument.Tree;
with Strings;         use Strings;
with Text_Files;

package body Instrument.Sources is

   package LAL renames Libadalang.Analysis;

   function Create_Identifier
     (RH : Rewriting_Handle; Text : Text_Type) return Node_Rewriting_Handle
   is (Create_Token_Node (RH, Libadalang.Common.Ada_Identifier, Text));

   function To_Nodes
     (Handle : Rewriting_Handle;
      Name   : Ada_Qualified_Name) return Node_Rewriting_Handle
      with Pre => not Name.Is_Empty;
   --  Turn the given qualified name into a name tree for rewriting

   package Ada_Qualified_Name_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada_Qualified_Name,
      "="          => Ada_Identifier_Vectors."=");

   function Buffer_Units_For_Closure
     (IC   : Inst_Context;
      Main : Ada_Qualified_Name)
      return Ada_Qualified_Name_Vectors.Vector;
   --  Return the list of buffer units names for all units of interest in
   --  Main's closure. If for some reason we cannot get this list, just return
   --  an empty one.

   -------------------------------------
   -- Generation of witness fragments --
   -------------------------------------

   function Make_Decision_Witness
     (IC         : Unit_Inst_Context;
      Bits       : Decision_Bit_Ids;
      MCDC_State : Unbounded_String;
      Decision   : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a function call to witness the outcome of the given decision,
   --  to be recorded using the given bit ids. If MC/DC is requested,
   --  MCDC_State is the name of the MC/DC state local variable, else it
   --  is the empty string.

   function Make_Condition_Witness
     (IC         : Unit_Inst_Context;
      MCDC_State : Unbounded_String;
      Condition  : Node_Rewriting_Handle;
      Offset     : Natural;
      First      : Boolean) return Node_Rewriting_Handle;
   --  Create a function call to witness the value of the given condition,
   --  to be recorded in the given MC/DC state local variable.

   procedure Insert_Condition_Witness
     (IC     : in out Unit_Inst_Context;
      SC     : Source_Condition;
      Offset : Natural);
   --  For use when MC/DC is requested. Insert witness function call for the
   --  identified condition.

   procedure Insert_Decision_Witness
     (IC         : in out Unit_Inst_Context;
      SD         : Source_Decision;
      Path_Count : Positive);
   --  For use when decision coverage or MC/DC is requested. Insert witness
   --  function call for the identified condition.

   ---------------------------
   -- Make_Decision_Witness --
   ---------------------------

   function Make_Decision_Witness
     (IC         : Unit_Inst_Context;
      Bits       : Decision_Bit_Ids;
      MCDC_State : Unbounded_String;
      Decision   : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
      E        : Instrumentation_Entities renames IC.Entities;

      Is_MCDC : constant Boolean := Bits.Path_Bits_Base /= No_Bit_Id;

      --  Note: we can't pass Decision directly as a subsitution to
      --  Create_From_Template, as this would unparse it and create a
      --  complete new tree, whereas we want to preserve the original
      --  tree so that we can instrument individual conditions for MC/DC.

      Call_Img : constant String :=
        "{}.Witness ({}"
        & "," & Img (Bits.Outcome_Bits (False))
        & "," & Img (Bits.Outcome_Bits (True))
        & (if Is_MCDC
           then ", {}"
                & ", " & Img (Bits.Path_Bits_Base)
                & ", " & To_String (MCDC_State) & "'Address"
           else "")
        & ")";

      RH_Call : constant Node_Rewriting_Handle :=
        Create_From_Template
          (IC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Call_Img),
           Arguments => (1 => E.Common_Buffers,
                         2 => E.Decision_Buffer)
                        & (if Is_MCDC
                           then (1 => E.MCDC_Buffer)
                           else (1 .. 0 => No_Node_Rewriting_Handle)),
           Rule      => Expr_Rule);

   begin
      --  The second child of RH_Call is its list of actual parameters

      Append_Child (Child (RH_Call, 2), Decision);
      return RH_Call;
   end Make_Decision_Witness;

   ----------------------------
   -- Make_Condition_Witness --
   ----------------------------

   function Make_Condition_Witness
     (IC         : Unit_Inst_Context;
      MCDC_State : Unbounded_String;
      Condition  : Node_Rewriting_Handle;
      Offset     : Natural;
      First      : Boolean) return Node_Rewriting_Handle
   is
      E        : Instrumentation_Entities renames IC.Entities;
      Call_Img : constant String :=
        "{}.Witness (" & To_String (MCDC_State) & "'Address,"
        & Img (Offset) & "," & First'Img & ")";

      RH_Call : constant Node_Rewriting_Handle :=
        Create_From_Template
          (IC.Rewriting_Context,
           Template  => To_Wide_Wide_String (Call_Img),
           Arguments => (1 => E.Common_Buffers),
           Rule      => Expr_Rule);

   begin
      --  The second child of RH_Call is its list of actual parameters

      Append_Child (Child (RH_Call, 2), Condition);
      return RH_Call;
   end Make_Condition_Witness;

   --------------
   -- To_Nodes --
   --------------

   function To_Nodes
     (Handle : Rewriting_Handle;
      Name   : Ada_Qualified_Name) return Node_Rewriting_Handle
   is
      Result : Node_Rewriting_Handle := No_Node_Rewriting_Handle;
   begin
      for Id of Name loop
         declare
            Id_Node : constant Node_Rewriting_Handle := Create_Identifier
              (Handle, To_Text (To_String (Id)));
         begin
            if Result = No_Node_Rewriting_Handle then
               Result := Id_Node;
            else
               Result := Create_Dotted_Name (Handle, Result, Id_Node);
            end if;
         end;
      end loop;
      return Result;
   end To_Nodes;

   --------------------------
   -- Initialize_Rewriting --
   --------------------------

   procedure Initialize_Rewriting
     (IC                : out Unit_Inst_Context;
      Instrumented_Unit : Compilation_Unit_Name;
      Context           : Analysis_Context) is
   begin
      IC.Instrumented_Unit := Instrumented_Unit;
      IC.Buffer_Unit := (Buffer_Unit (Instrumented_Unit), Unit_Spec);
      IC.Pure_Buffer_Unit := (Pure_Buffer_Unit (Instrumented_Unit), Unit_Spec);
      IC.Rewriting_Context := Handle (Context);

      declare
         RH : constant Rewriting_Handle := IC.Rewriting_Context;
         E  : Instrumentation_Entities renames IC.Entities;
      begin
         E.Common_Buffers := To_Nodes (RH, Sys_Buffers);
         E.Unit_Buffers := To_Nodes (RH, IC.Pure_Buffer_Unit.Unit);
         E.Statement_Buffer :=
            To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & Statement_Buffer_Name);

         if Coverage.Enabled (Decision) or else MCDC_Coverage_Enabled then
            E.Decision_Buffer :=
              To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & Decision_Buffer_Name);

            if MCDC_Coverage_Enabled then
               E.MCDC_Buffer :=
                 To_Nodes (RH, IC.Pure_Buffer_Unit.Unit & MCDC_Buffer_Name);
            end if;
         end if;
      end;
   end Initialize_Rewriting;

   ------------------------------
   -- Buffer_Units_For_Closure --
   ------------------------------

   function Buffer_Units_For_Closure
     (IC   : Inst_Context;
      Main : Ada_Qualified_Name)
      return Ada_Qualified_Name_Vectors.Vector
   is
      pragma Unreferenced (Main);
      Result : Ada_Qualified_Name_Vectors.Vector;
   begin
      --  TODO??? Here, we need the list of files needed to build Main: specs
      --  for units WITHed by main, their bodies, the separates, etc.  It's
      --  unclear what GNATCOLL.Projects.Get_Closure does, but experimentations
      --  show that it's not what we want. So for now, return an approximation:
      --  buffer units for all instrumented units. In the future, we should
      --  either get this service from GNATCOLL.Projects, either re-implement
      --  it on top of Libadalang.

      for Cur in IC.Instrumented_Units.Iterate loop
         declare
            Instr_Unit : constant Compilation_Unit_Name :=
               Instrumented_Unit_Maps.Key (Cur);
         begin
            Result.Append (Buffer_Unit (Instr_Unit));
         end;
      end loop;
      return Result;
   end Buffer_Units_For_Closure;

   ---------------------------
   -- Add_Auto_Dump_Buffers --
   ---------------------------

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Main : Ada_Qualified_Name;
      URH  : Unit_Rewriting_Handle)
   is
      No_Node : Node_Rewriting_Handle renames No_Node_Rewriting_Handle;

      U   : constant Analysis_Unit := Unit (URH);
      RH  : constant Rewriting_Handle := Handle (U.Context);
      Tmp : LAL.Ada_Node := U.Root;

      CU        : LAL.Compilation_Unit;
      Subp_Body : LAL.Subp_Body;

      Old_Stmts, New_Stmts : Node_Rewriting_Handle;

      New_Stmt_List        : constant Node_Rewriting_Handle :=
         Create_Node (RH, Ada_Stmt_List);
      --  List of statements to contain 1) the original handled statements
      --  (Old_Stmts) and 2) the call to the Write_Trace_File procedure.

      Output_Unit, Output_Proc : Ada_Qualified_Name;
      --  Qualified names for the unit that contains the buffer output
      --  procedure, and for the procedure itself.

      Buffer_Id : constant Node_Rewriting_Handle :=
         Create_Identifier (RH, "Buffers");
      Access_Id : constant Node_Rewriting_Handle :=
         Create_Identifier (RH, "Access");

      Buffer_Units : constant Ada_Qualified_Name_Vectors.Vector :=
         Buffer_Units_For_Closure (IC, Main);
      --  List of names for units that contains the buffers to dump

   begin
      if Buffer_Units.Is_Empty then
         return;
      end if;

      --  Make sure this main source has the expected structure: a
      --  simple subprogram body in a compilation unit. If not, return without
      --  doing anything.

      if Tmp.Kind /= Ada_Compilation_Unit then
         return;
      else
         CU := Tmp.As_Compilation_Unit;
      end if;

      Tmp := CU.F_Body;
      if Tmp.Kind /= Ada_Library_Item then
         return;
      end if;

      Tmp := Tmp.As_Library_Item.F_Item.As_Ada_Node;
      if Tmp.Kind /= Ada_Subp_Body then
         return;
      else
         Subp_Body := Tmp.As_Subp_Body;
      end if;

      --  Compute the qualified names we need for instrumentation

      Output_Unit := Sys_Prefix;
      Output_Unit.Append (To_Unbounded_String ("Traces"));
      Output_Unit.Append (To_Unbounded_String ("Output"));

      Output_Proc := Output_Unit;
      Output_Proc.Append (To_Unbounded_String ("Write_Trace_File"));

      --  Add the required WITH clauses

      declare
         Prelude : constant Node_Rewriting_Handle := Handle (CU.F_Prelude);

         procedure Add_With (Unit : Ada_Qualified_Name);
         --  Add a WITH clause to Prelude for the given unit name

         --------------
         -- Add_With --
         --------------

         procedure Add_With (Unit : Ada_Qualified_Name) is
            With_Clause : constant Node_Rewriting_Handle :=
               Create_From_Template
                 (RH,
                  Template  => "with {};",
                  Arguments => (1 => To_Nodes (RH, Unit)),
                  Rule      => With_Clause_Rule);
         begin
            Append_Child (Prelude, With_Clause);
         end Add_With;

      begin
         Add_With (Output_Unit);
         for Buffer_Unit of Buffer_Units loop
            Add_With (Buffer_Unit);
         end loop;
      end;

      --  Wrap the previous subprogram body content (declarations, handled
      --  statements) in a declare block. This is a simple handled statements
      --  block if there is no declaration.

      declare
         New_Excs : constant Node_Rewriting_Handle :=
            Create_Node (RH, Ada_Ada_Node_List);

         Nested_Block : Node_Rewriting_Handle;
         Nested_Decls : Node_Rewriting_Handle;
      begin
         --  Extract the original statements (Old_Stmts) and replace it in the
         --  subprogram body with the new statements.

         Old_Stmts := Handle (Subp_Body.F_Stmts);
         New_Stmts := Create_Regular_Node
           (RH, Ada_Handled_Stmts, (New_Stmt_List, New_Excs));
         Replace (Old_Stmts, New_Stmts);

         --  If the original subprogram has declarations, wrap the original
         --  statements in a declare block to hold them.

         if Subp_Body.F_Decls.F_Decls.Children_Count = 0 then
            Nested_Block := Old_Stmts;
         else
            Nested_Decls := Handle (Subp_Body.F_Decls);
            Replace
              (Nested_Decls,
               Create_Regular_Node
                 (RH, Ada_Declarative_Part,
                  (1 => Create_Node (RH, Ada_Ada_Node_List))));

            Nested_Block := Create_Regular_Node
              (RH, Ada_Decl_Block,
               (Nested_Decls, --  F_Decls
                Old_Stmts,    --  F_Stmts
                No_Node));    --  F_End_Name
         end if;

         Append_Child (New_Stmt_List, Nested_Block);
      end;

      --  Build the call to Write_Trace_File on the list of buffers, and append
      --  it to New_Stmt_List, right after the old list of statements.

      declare
         use Ada_Qualified_Name_Vectors;

         Buffer_List : constant Node_Rewriting_Handle :=
            Create_Node (RH, Ada_Assoc_List);
         --  List of Ada_Aggregate_Assoc nodes for the list of buffers to pass
         --  to the output procedure.

         Aggregate : constant Node_Rewriting_Handle := Create_Regular_Node
           (RH, Ada_Aggregate, (1 => No_Node,       --  F_Ancestor_Expr
                                2 => Buffer_List)); --  F_Assocs

         Param_Assoc : constant Node_Rewriting_Handle := Create_Regular_Node
           (RH, Ada_Param_Assoc, (1 => No_Node,     --  F_Designator
                                  2 => Aggregate)); --  F_Expr

         Assoc_List : constant Node_Rewriting_Handle :=
            Create_Node (RH, Ada_Assoc_List);

         Call_Expr : constant Node_Rewriting_Handle := Create_Regular_Node
           (RH, Ada_Call_Expr, (1 => To_Nodes (RH, Output_Proc), --  F_Name
                                2 => Assoc_List));               --  F_Suffix

         Call_Stmt : constant Node_Rewriting_Handle :=
            Create_Regular_Node (RH, Ada_Call_Stmt, (1 => Call_Expr));
      begin
         for Cur in Buffer_Units.Iterate loop
            declare
               Buffer_Name : constant Node_Rewriting_Handle :=
                  Create_Regular_Node
                    (RH, Ada_Dotted_Name,
                     (1 => To_Nodes (RH, Element (Cur)), --  F_Prefix
                      2 => Clone (Buffer_Id)));          --  F_Suffix

               Buffer_Access : constant Node_Rewriting_Handle :=
                  Create_Regular_Node
                    (RH, Ada_Attribute_Ref,
                     (1 => Buffer_Name,         --  F_Prefix
                      2 => Clone (Access_Id),   --  F_Attribute
                      3 => No_Node));           --  F_Args

               Designator      : constant Node_Rewriting_Handle :=
                  Create_Node (RH, Ada_Alternatives_List);
               Designator_Text : constant Text_Type :=
                  To_Text (Strings.Img (To_Index (Cur)));

               Assoc : constant Node_Rewriting_Handle :=
                  Create_Regular_Node
                    (RH, Ada_Aggregate_Assoc,
                     (1 => Designator,      --  F_Designators
                      2 => Buffer_Access)); --  F_R_Expr
            begin
               Append_Child
                 (Designator,
                  Create_Token_Node (RH, Ada_Int_Literal, Designator_Text));
               Append_Child (Buffer_List, Assoc);
            end;
         end loop;
         Append_Child (Assoc_List, Param_Assoc);
         Append_Child (New_Stmt_List, Call_Stmt);
      end;
   end Add_Auto_Dump_Buffers;

   ------------------------------
   -- Insert_Condition_Witness --
   ------------------------------

   procedure Insert_Condition_Witness
     (IC     : in out Unit_Inst_Context;
      SC     : Source_Condition;
      Offset : Natural)
   is
      N : Expr renames SC.Condition;

      RH_P : constant Node_Rewriting_Handle :=
        Create_Node
          (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);
      RH_N : constant Node_Rewriting_Handle := Handle (N);

   begin
      --  Detach original condition from tree so that it can be reattached
      --  inside the witness call.

      Replace (RH_N, RH_P);

      --  Now attach witness call at the place of the original condition

      Replace
        (RH_P,
         Make_Condition_Witness (IC, SC.State, RH_N, Offset, SC.First));
   end Insert_Condition_Witness;

   -----------------------------
   -- Insert_Decision_Witness --
   -----------------------------

   procedure Insert_Decision_Witness
     (IC         : in out Unit_Inst_Context;
      SD         : Source_Decision;
      Path_Count : Positive)
   is
      LL_SCO_Id : Nat renames SD.LL_SCO;
      N         : Expr renames SD.Decision;

      Bits : Decision_Bit_Ids;
      RH_P : constant Node_Rewriting_Handle :=
        Create_Node
          (IC.Rewriting_Context, Libadalang.Common.Ada_Identifier);

      RH_N : constant Node_Rewriting_Handle := Handle (N);
   begin
      Bits.LL_D_SCO := LL_SCO_Id;

      --  Allocate outcome bits

      Bits.Outcome_Bits :=
        (False => IC.Unit_Bits.Last_Outcome_Bit + 1,
         True  => IC.Unit_Bits.Last_Outcome_Bit + 2);
      IC.Unit_Bits.Last_Outcome_Bit :=
        IC.Unit_Bits.Last_Outcome_Bit + 2;

      --  Allocate path bits for MC/DC

      if MCDC_Coverage_Enabled then
         Bits.Path_Bits_Base := IC.Unit_Bits.Last_Path_Bit + 1;
         IC.Unit_Bits.Last_Path_Bit :=
           IC.Unit_Bits.Last_Path_Bit + Bit_Id (Path_Count);
      else
         Bits.Path_Bits_Base := No_Bit_Id;
      end if;

      IC.Unit_Bits.Decision_Bits.Append (Bits);

      --  Detach original decision from tree so that it can be reattached
      --  inside the witness call.

      Replace (RH_N, RH_P);

      --  Now attach witness call at the place of the original decision

      Replace (RH_P,
        Make_Decision_Witness (IC, Bits, SD.State, RH_N));
   end Insert_Decision_Witness;

   ----------------------------
   -- Instrument_Source_File --
   ----------------------------

   procedure Instrument_Source_File
     (CU_Name   : Compilation_Unit_Name;
      Unit_Info : Instrumented_Unit_Info;
      Prj_Info  : in out Project_Info;
      IC        : in out Inst_Context;
      UIC       : out Unit_Inst_Context)
   is
      Rewriter : Source_Rewriter;
      Filename : constant String := To_String (Unit_Info.Filename);

      Preelab : constant Boolean := False;
      --  ??? To be implemented in Libadalang: S128-004
      --
      --  Set to True if Unit is required to be preelaborable, i.e.  it is
      --  either preelaborated, or the declaration of a remote types or
      --  remote call interface library unit. In this case, do not generate
      --  any witness calls for elaboration of declarations: they would be
      --  pointless (there is no elaboration code anyway) and, in any case,
      --  illegal.

   begin
      Rewriter.Start_Rewriting (Prj_Info, Filename);

      Initialize_Rewriting (UIC, CU_Name, Rewriter.Rewritten_Context);

      UIC.SFI := Get_Index_From_Generic_Name
        (Filename, Kind => Files_Table.Source_File);
      UIC.CU := Allocate_CU (Provider => Instrumenter, Origin => UIC.SFI);
      --  In the instrumentation case, the origin of SCO information is
      --  the original source file.

      --  Then run SCOs generation. This inserts calls to witness
      --  procedures/functions in the same pass.

      SCOs.Initialize;
      Traverse_Declarations_Or_Statements
        (IC      => UIC,
         P       => Rewriter.Rewritten_Unit.Root,
         L       => No_Ada_Node_List,
         Preelab => Preelab);

      SCOs.SCO_Unit_Table.Append
        ((File_Name  => new String'(Filename),
          File_Index => UIC.SFI,
          Dep_Num    => 1,
          From       => SCOs.SCO_Table.First,
          To         => SCOs.SCO_Table.Last));

      --  Convert low level SCOs from the instrumenter to high level SCOs.
      --  This creates BDDs for every decision.

      declare
         SCO_Map : aliased LL_HL_SCO_Map :=
           (SCOs.SCO_Table.First .. SCOs.SCO_Table.Last => No_SCO_Id);
         Bit_Maps : CU_Bit_Maps;
      begin
         Process_Low_Level_SCOs
           (UIC.CU,
            UIC.SFI,
            SCO_Map => SCO_Map'Access);

         if Coverage.Enabled (Coverage.Decision)
           or else MCDC_Coverage_Enabled
         then
            for SD of UIC.Source_Decisions loop
               Insert_Decision_Witness
                 (UIC, SD, Path_Count (SCO_Map (SD.LL_SCO)));
            end loop;

            if MCDC_Coverage_Enabled then
               --  As high-level SCO tables have been populated, we have built
               --  BDDs for each decisions, and we can now set the correct
               --  MC/DC path offset for each condition.

               for SC of UIC.Source_Conditions loop
                  Insert_Condition_Witness
                    (UIC, SC, Offset_For_True (SCO_Map (SC.LL_SCO)));
               end loop;
            end if;
         end if;

         --  Witnesses have now been inserted, and bit indices allocated: build
         --  bit maps.

         Bit_Maps :=
           (Statement_Bits => new Statement_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Statement_Bit => No_SCO_Id),
            Decision_Bits  => new Decision_Bit_Map'
              (Bit_Id'First .. UIC.Unit_Bits.Last_Outcome_Bit =>
                   (No_SCO_Id, False)),
            MCDC_Bits      =>
               new MCDC_Bit_Map'(Bit_Id'First .. UIC.Unit_Bits.Last_Path_Bit =>
                                     (No_SCO_Id, 0)));

         for S_Bit_Alloc of UIC.Unit_Bits.Statement_Bits loop
            Bit_Maps.Statement_Bits (S_Bit_Alloc.Executed) :=
              SCO_Map (S_Bit_Alloc.LL_S_SCO);
         end loop;

         for D_Bit_Alloc of UIC.Unit_Bits.Decision_Bits loop
            declare
               D_SCO : constant SCO_Id := SCO_Map (D_Bit_Alloc.LL_D_SCO);
            begin
               for Outcome in Boolean loop
                  Bit_Maps.Decision_Bits
                    (D_Bit_Alloc.Outcome_Bits (Outcome)) :=
                      (D_SCO, Outcome);
               end loop;

               if MCDC_Coverage_Enabled then
                  declare
                     Path_Count : constant Natural :=
                       SC_Obligations.Path_Count (D_SCO);
                  begin
                     for J in 1 .. Any_Bit_Id (Path_Count) loop
                        Bit_Maps.MCDC_Bits
                          (D_Bit_Alloc.Path_Bits_Base + J - 1) :=
                          (D_SCO, Natural (J - 1));
                     end loop;
                  end;
               end if;
            end;
         end loop;

         Set_Bit_Maps (UIC.CU, Bit_Maps);
      end;

      --  Insert automatic buffer dump calls, if requested

      if IC.Auto_Dump_Buffers and then Unit_Info.Is_Main then
         Add_Auto_Dump_Buffers
           (IC   => IC,
            Main => UIC.Instrumented_Unit.Unit,
            URH  => Handle (Rewriter.Rewritten_Unit));
      end if;

      --  Emit the instrumented source file

      Rewriter.Apply;
   end Instrument_Source_File;

end Instrument.Sources;
