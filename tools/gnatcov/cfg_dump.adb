------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces;

with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib;

with Binary_Files; use Binary_Files;
with Coverage.Source;
with Decision_Map;
with Diagnostics;  use Diagnostics;
with Disassemblers;
with Elf_Disassemblers;
with Execs_Dbase;
with Hex_Images;   use Hex_Images;
with Highlighting;
with Outputs;      use Outputs;
with Qemu_Traces;
with SC_Obligations;
with Strings;
with Switches;
with Traces;       use Traces;
with Traces_Dbase;
with Traces_Elf;   use Traces_Elf;
with Traces_Files;

package body CFG_Dump is

   use type Pc_Type;

   --------------------
   -- Output helpers --
   --------------------

   --  There are two kinds of output to handle. The output to a file is
   --  handled with Ada.Text_IO, and the output to a subprocess is handled
   --  with GNAT.Expect. These two packages have incompatible interfaces with
   --  respect to text output, so there is a need for an abstraction layer.

   type Output_Kind is (File, Subprocess);
   type Output_File_Access is access all File_Type;
   type Output_Type is record
      Kind     : Output_Kind;

      File     : Output_File_Access;
      --  Used when Kind => File

      Pd       : Process_Descriptor;
      --  Used when Kind => Subprogram

      To_Close : Boolean;
      --  Whether the output must be closed. When Kind => File and File =>
      --  Standard_Output, this musn't be closed.
   end record;

   procedure Put (Output : in out Output_Type; C : Character);
   procedure Put (Output : in out Output_Type; S : String);
   procedure Put_Line (Output : in out Output_Type; S : String);
   procedure Close (Output : in out Output_Type);

   -------------------------
   -- CFG data structures --
   -------------------------

   type Successor_Kind is (Fallthrough, Branch, Subp_Return, Raise_Exception);

   type Successor_Record is record
      Kind    : Successor_Kind;

      Known   : Boolean;
      --  Whether the address of this successor instruction is known (i.e if it
      --  is not the target of an indirect branch).

      Address : Pc_Type;
      --  Address of this successor instruction
   end record;
   --  Instructions are executed as sequences, and the execution flow can
   --  sometimes jump. This type is used to describe the possible successors
   --  of some instruction during the execution.

   package Successor_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Successor_Record);

   package Boolean_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Boolean);

   package PC_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Pc_Type,
      "<"          => "<",
      "="          => "=");
   subtype PC_Set is PC_Sets.Set;

   type Instruction_Record;
   type Instruction_Access is access all Instruction_Record;

   type Instruction_Record is record
      Bytes               : Binary_Content;
      --  Location and content for this instruction

      Section             : Binary_Files.Section_Index;
      --  Index of the section in which this instruction was found

      Insn_Set            : Elf_Disassemblers.Insn_Set_Type;

      Selected            : Boolean;
      --  Whether this instruction matches selected locations

      Executed            : Boolean;
      --  Whether this instructions was executed when producing traces

      Control_Flow_Pair   : Pc_Type;
      --  If this instruction has control flow effects, contains the address
      --  of the last instructions executed before the effects are applied.
      --  On architectures with no delay slot, this will be this instruction
      --  itself.
      --
      --  If this instruction ends a basic block because of control flow
      --  effects applying to it, contains the address of the instruction these
      --  effects come from. On architectures with no delay slot, this will be
      --  this instruction itself.
      --
      --  In all other cases, contains No_PC.

      Successors          : Successor_Vectors.Vector;
      --  Set of instructions that can be executed right after this one (with
      --  fallthrough, branch, call, etc.).

      Executed_Successors : Boolean_Vectors.Vector;
      --  For each successor, whether the corresponding branch has been taken
   end record;

   function Has_Lower_Address
     (Left, Right : Instruction_Access) return Boolean is
     (Left.Bytes.First < Right.Bytes.First);
   function Has_Same_Address
     (Left, Right : Instruction_Access) return Boolean is
     (Left.Bytes.First = Right.Bytes.First);
   function Address (Insn : Instruction_Access) return Pc_Type is
     (Insn.Bytes.First);
   procedure Free is new Ada.Unchecked_Deallocation
     (Instruction_Record, Instruction_Access);

   package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Instruction_Access);
   subtype Instructions_Vector is Instruction_Vectors.Vector;

   package Instruction_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Instruction_Access,
      "<"          => Has_Lower_Address,
      "="          => Has_Same_Address);
   subtype Instructions_Set is Instruction_Sets.Set;

   subtype Basic_Block is Instructions_Vector;
   --  A basic block gathers multiple instructions that are executed in
   --  a sequence. The execution flow always starts executing the first
   --  instruction of a basic block and cannot stop in the middle of it except
   --  because of a trap. Note that basic blocks always contain at least one
   --  instruction.

   type Basic_Block_Access is access all Basic_Block;

   procedure Free is new Ada.Unchecked_Deallocation
     (Basic_Block, Basic_Block_Access);

   package Basic_Block_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Pc_Type,
      Element_Type => Basic_Block_Access);
   subtype Basic_Blocks is Basic_Block_Maps.Map;

   function Address (BB : Basic_Block) return Pc_Type is
      (BB.First_Element.Bytes.First);
   --  Return the address of the first instruction of a basic block

   function Successors (BB : Basic_Block) return Successor_Vectors.Vector is
     (BB.Last_Element.Successors);
   --  Return the successors of the last instruction of a basic block

   function Get_Instruction
     (CFG     : Basic_Blocks;
      Address : Pc_Type) return Instruction_Access;
   --  Return the instruction whose first byte is Address, or null if there is
   --  no one.

   type Context_Type is record
      Exec : Exe_File_Acc;
      --  Executable instructions come from

      Locs : Proc_Locations;
      --  Locations used to filter instructions to display

      Instructions : Instructions_Vector;
      --  Temporary list of instructions to display. Built before building the
      --  CFG (see Collect_Instructions).

      Basic_Block_Starters : PC_Set;
      --  Set of instructions that have to start a basic block

      Other_Outcome, Other_Income : Instructions_Set;
      --  Outcome: keys are not selected instructions that branch/have
      --  fallthrough to a selected instruction. Associated values are
      --  these selected instructions. Income: reciprocal.

      CFG                : Basic_Blocks;
      Group_By_Condition : Boolean;
      Keep_Edges         : Boolean;
      Tag_Executed       : Boolean;

      Output : Output_Type;
      --  Output file to write the CFG to
   end record;
   --  Group of information used to compute the CFG

   type Context_Access is access all Context_Type;

   procedure Collect_Instructions
     (Context : Context_Access;
      Section : Address_Info_Acc);
   --  Complete Context.Instructions with instructions found in Section that
   --  match provided locations.
   --  This disassembles all of Section, not just the specified locations,
   --  which is a hughe waste and means that any issue impacting disassembly
   --  of any symbol in the section can potentially hinder dumping the CFG
   --  for an entirely unrelated symbol, even if the offending location is
   --  outside of the specified range???

   procedure Build_Basic_Blocks (Context : Context_Access);
   --  Group Context.Instructions into Context.Basic_Blocks. Make
   --  Context.Instructions empty before returning.

   procedure Clear_Basic_Blocks (Context : Context_Access);
   --  Deallocate basic blocks and contained instructions

   procedure Tag_Executed_Instructions
     (Context           : Context_Access;
      Exec_Path         : String;
      Traces_Files_List : Inputs.Inputs_Type);
   --  Extract traces from each file and use them to tag instructions if they
   --  have been executed. This pass works on the CFG.

   procedure Output_CFG (Context : Context_Access);
   --  Actually output the CFG in the dot format to the output

   ------------
   -- Colors --
   ------------

   Hex_Color      : constant Highlighting.Color_Type := "808080";

   Edge_Unselected_Color : constant Highlighting.Color_Type := "808080";
   Edge_Default_Color    : constant Highlighting.Color_Type := "800000";
   Edge_Executed_Color   : constant Highlighting.Color_Type := "008000";
   Unexecuted_Insn_Color : constant Highlighting.Color_Type := "800000";
   Executed_Insn_Color   : constant Highlighting.Color_Type := "008000";
   Sloc_Color            : constant Highlighting.Color_Type := "004080";

   Unknown_Color : constant Highlighting.Color_Type := "a0a0a0";
   True_Color    : constant Highlighting.Color_Type := "008000";
   False_Color   : constant Highlighting.Color_Type := "800000";

   ---------------------
   -- Get_Instruction --
   ---------------------

   function Get_Instruction
     (CFG     : Basic_Blocks;
      Address : Pc_Type) return Instruction_Access
   is
      use Basic_Block_Maps;

      BB : constant Cursor := CFG.Floor (Address);
   begin
      if BB /= No_Element then
         for Insn of Element (BB).all loop
            if CFG_Dump.Address (Insn) = Address then
               return Insn;
            end if;
         end loop;
      end if;
      return null;
   end Get_Instruction;

   --------------------------
   -- Collect_Instructions --
   --------------------------

   procedure Collect_Instructions
     (Context : Context_Access;
      Section : Address_Info_Acc)
   is
      Code    : constant Binary_Content := Section.Section_Content;
      Sec_Ndx : constant Binary_Files.Section_Index := Section.Section_Sec_Idx;

      I_Ranges : Elf_Disassemblers.Insn_Set_Ranges renames
        Get_Insn_Set_Ranges (Context.Exec.all, Sec_Ndx).all;
      Cache    : Elf_Disassemblers.Insn_Set_Cache :=
        Elf_Disassemblers.Empty_Cache;
      Disas    : access Disassemblers.Disassembler'Class;
      Insn_Set : Elf_Disassemblers.Insn_Set_Type;

      type Disassembly_State is
        (OK,
         Invalid_Insn,
         Skip_Padding);
      State : Disassembly_State;
      --  Disassembled instructions

      Saved_Exn : Exception_Occurrence;
      --  When disassembling fails, there is a need to delay the exception
      --  handling: this variable is used to keep exception information.

      PC      : Pc_Type := Code.First;
      --  Address of the first byte of the instruction we are about to
      --  disassemble.

      Old_PC  : Pc_Type;
      --  Likewise for the previous instruction

      Sym_Cur : Address_Info_Sets.Cursor :=
        Find_Address_Info (Context.Exec.all, Symbol_Addresses, PC);
      --  Cursor to the symbol that contains PC, if it exists, or to the first
      --  symbol after PC. If there is no such symbol, No_Element.

      Symbol  : Address_Info_Acc;
      --  Symbol corresponding to Sym_Cur, on null if it is No_Element

      Insn, Last_Insn : Instruction_Access := null;
      --  Currently analyzed instructions and the previous one. Last_Insn is
      --  set to null when it has no fallthrough.

      Insn_Added, Last_Insn_Added : Boolean := False;
      --  Whether the current/previous instruction is referenced in the
      --  database. Used to know what to deallocate.

      Insn_Len        : Positive;
      --  Length of the current instruction

      Insn_Branch                     : Branch_Kind;
      Insn_Flag_Indir, Insn_Flag_Cond : Boolean;
      Insn_Branch_Dest, Insn_FT_Dest  : Disassemblers.Dest;
      Insn_Has_FT                     : Boolean := False;
      Insn_Starts_BB                  : Boolean := False;

      type Branch_Point_Record is record
         Branch_Instruction : Pc_Type;
         --  Instruction this branch point comes from

         Address            : Pc_Type;
         --  Address of the last instruction that is executed before the branch
         --  actually happens.

         Target             : Successor_Record;
         --  Describes the effect of this branch

         Is_Conditional     : Boolean;
         --  Whether is a conditional branch
      end record;
      --  Holds information about the effect of a branching instruction

      package Branch_Point_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Branch_Point_Record);
      use Branch_Point_Lists;

      Branch_Points : Branch_Point_Lists.List;
      --  Stores data about the effect of analyzed branch instructions. These
      --  can be postponed because of delay slots. Branch points are removed
      --  from this list once the point they apply to has been reached.

      Cur : Branch_Point_Lists.Cursor;

      procedure Add_Branch_Point
        (Address, Target, Delay_Slot : Pc_Type;
         Effect                      : Successor_Kind;
         Is_Conditional              : Boolean);
      --  Add a branch point to the list

      ----------------------
      -- Add_Branch_Point --
      ----------------------

      procedure Add_Branch_Point
        (Address, Target, Delay_Slot : Pc_Type;
         Effect                      : Successor_Kind;
         Is_Conditional              : Boolean) is
      begin
         Branch_Points.Append
           ((Branch_Instruction => Address,
             Address            =>
                 (if Delay_Slot = No_PC
                  then Address
                  else Delay_Slot),
             Target             =>
               (Kind    => Effect,
                Known   => Target /= No_PC,
                Address => Target),
             Is_Conditional     => Is_Conditional));
      end Add_Branch_Point;

   --  Start of processing for Collect_Instructions

   begin

      while Elf_Disassemblers.Iterate_Over_Insns
        (I_Ranges, Cache, Code.Last, PC, Insn_Set)
      loop
         Disas := Elf_Disassemblers.Disa_For_Machine
           (Traces.Machine, Insn_Set);

         if Insn = null or else Insn_Added then
            Insn := new Instruction_Record'
              (Bytes    => (null, PC, No_PC),
               Section  => Sec_Ndx,
               Insn_Set => Insn_Set,
               others   => <>);
            Insn_Added := False;
         end if;

         --  Adjust Sym according to PC; Symbol as well

         while Address_Info_Sets."/=" (Sym_Cur, Address_Info_Sets.No_Element)
           and then Address_Info_Sets.Element (Sym_Cur).Last < PC
         loop
            Sym_Cur := Address_Info_Sets.Next (Sym_Cur);
         end loop;
         Symbol :=
           (if Address_Info_Sets."=" (Sym_Cur, Address_Info_Sets.No_Element)
            then null
            else Address_Info_Sets.Element (Sym_Cur));

         --  First decode the current instruction

         begin
            State := OK;
            Insn_Len := Disas.Get_Insn_Length
              (Slice (Code, PC, Code.Last));
            Insn.Bytes := Slice
              (Code, PC, PC + Pc_Type (Insn_Len) - 1);

            if Symbol /= null then
               if Insn.Bytes.First < Symbol.First
                 and then Symbol.First <= Insn.Bytes.Last
               then
                  --  This instruction crosses a symbol's lower address: it
                  --  looks like we are disassembling padding bytes. Skip
                  --  these until this symbol.

                  State := Skip_Padding;

               elsif Insn.Bytes.First <= Symbol.Last
                 and then Symbol.Last < Insn.Bytes.Last
               then
                  --  This instruction crosses a symbol's upper address: we
                  --  assume instructions are not supposed to do that.

                  State := Invalid_Insn;

                  --  Raise an exception: this is the only way to craft an
                  --  Exception_Occurrence object...

                  begin
                     raise Disassemblers.Invalid_Insn
                     with "Insn crosses a symbol's upper address";
                  exception
                     when Exn : Disassemblers.Invalid_Insn =>
                        Save_Occurrence (Saved_Exn, Exn);
                  end;
               end if;
            end if;

            if State = OK then
               Disas.Get_Insn_Properties
                 (Insn_Bin    => Insn.Bytes,
                  Pc          => PC,
                  Branch      => Insn_Branch,
                  Flag_Indir  => Insn_Flag_Indir,
                  Flag_Cond   => Insn_Flag_Cond,
                  Branch_Dest => Insn_Branch_Dest,
                  FT_Dest     => Insn_FT_Dest);
            end if;

         exception
            when Exn : Disassemblers.Invalid_Insn
               | Disassemblers.Unhandled_Insn =>

               --  Keep in mind that we may end up here because of an error
               --  while computing the instruction's length. Thus, we cannot
               --  rely on the length (Insn_len) nor on the address of the
               --  last byte (Insn.Last).

               if Symbol = null
                 or else not (PC in Symbol.First .. Symbol.Last)
               then

                  --  We came across an invalid instruction and we are not
                  --  inside a symbol: we probably met padding bytes.

                  State := Skip_Padding;

               else

                  --  There is an invalid instruction inside a symbol: the code
                  --  generator looks buggy.

                  State := Invalid_Insn;
                  Save_Occurrence (Saved_Exn, Exn);
               end if;
         end;

         case State is
            when OK =>
               Insn.Selected := Matches_Locations
                 (Context.Exec, Context.Locs, Address (Insn));
               Insn.Executed := False;
               Insn.Control_Flow_Pair := No_PC;

               --  Past this point, PC contains the address of the next
               --  instruction. Use Address (Insn) to get the address of
               --  the current one.

               Old_PC := PC;
               PC := PC + Pc_Type (Insn_Len);

               --  Chain the previous instruction to this one only when needed

               if Last_Insn /= null then
                  Last_Insn.Successors.Append
                    ((Kind    => Fallthrough,
                      Known   => True,
                      Address => Address (Insn)));

                  if not Insn.Selected and then Last_Insn.Selected then
                     --  This instruction is not selected, but since it's
                     --  directly referenced by the previous instruction,
                     --  we are interested in it anyway.

                     Context.Other_Outcome.Include (Insn);
                     Insn_Added := True;

                  elsif Insn.Selected and then not Last_Insn.Selected then
                     --  Likewise, reciprocally

                     Context.Other_Income.Insert (Last_Insn);
                     Last_Insn_Added := True;
                  end if;
               end if;

               if Insn.Selected then
                  Context.Instructions.Append (Insn);
                  Insn_Added := True;

                  if Insn_Starts_BB then
                     Context.Basic_Block_Starters.Include (Address (Insn));
                  end if;
               end if;

               --  If this is a control flow instruction, memorize its effect
               --  to apply them after the delay slot.

               case Insn_Branch is
               when Br_Jmp =>
                  Add_Branch_Point
                    (Address (Insn),
                     Insn_Branch_Dest.Target,
                     Insn_Branch_Dest.Delay_Slot,
                     Branch,
                     Insn_Flag_Cond);
                  Insn.Control_Flow_Pair := Insn_Branch_Dest.Delay_Slot;

               when Br_Ret =>
                  Add_Branch_Point
                    (Address (Insn),
                     No_PC,
                     Insn_Branch_Dest.Delay_Slot,
                     Subp_Return,
                     Insn_Flag_Cond);

               when Br_Call =>
                  --  Calls usually have a fallthrough edge. However, calls
                  --  that raise an exception do have one.

                  if not Context.Keep_Edges then

                     --  Use debug information to get indirect call target, if
                     --  possible.

                     if Insn_Flag_Indir then
                        Insn_Branch_Dest.Target := Get_Call_Target
                          (Context.Exec.all,
                           Address (Insn),
                           Pc_Type (Insn_Len));
                     end if;

                     declare
                        Symbol : constant Address_Info_Acc := Get_Symbol
                          (Context.Exec.all, Insn_Branch_Dest.Target);
                        Symbol_Name : constant String_Access :=
                          (if Symbol = null
                           then null
                           else Symbol.Symbol_Name);
                     begin
                        if Symbol_Name /= null
                          and then
                            Decision_Map.Subp_Raises_Exception
                              (Symbol_Name.all)
                        then
                           Add_Branch_Point
                             (Address (Insn),
                              No_PC,
                              Insn_Branch_Dest.Delay_Slot,
                              Raise_Exception,
                              Insn_Flag_Cond);
                        end if;
                     end;
                  end if;

               when others =>
                  null;
               end case;

            when Invalid_Insn =>
               Disassemblers.Abort_Disassembler_Error
                 (PC, Slice (Code, PC, Code.Last), Saved_Exn);

            when Skip_Padding =>

               --  If there is no symbol anymore to re-synchronize the
               --  disassembling process, we consider that all remaining
               --  bytes are padding.

               if Symbol = null then
                  return;
               end if;

               Insn.Selected := False;
               PC := Symbol.First;

         end case;

         --  These are for the next instruction, during the next iteration.
         --  If the current instruction wasn't selected, be sure to break the
         --  basic block anyway: propagate the Insn_Starts_BB flag.

         Insn_Has_FT := State = OK;
         Insn_Starts_BB := Insn_Starts_BB and then not Insn.Selected;

         --  Apply branch points when needed

         Cur := Branch_Points.First;
         while Cur /= No_Element loop
            declare
               BP       : Branch_Point_Record renames Element (Cur);
               Next_Cur : constant Cursor := Next (Cur);
            begin
               if BP.Address < Address (Insn) then
                  Branch_Points.Delete (Cur);

               elsif BP.Address = Address (Insn) then
                  Insn_Has_FT := BP.Is_Conditional;
                  Insn_Starts_BB := True;

                  if BP.Target.Known then
                     if Matches_Locations
                       (Context.Exec, Context.Locs, BP.Target.Address)
                     then
                        Context.Basic_Block_Starters.Include
                          (BP.Target.Address);

                     elsif Insn.Selected then

                        --  The destination of this branch is out of scope, but
                        --  it is referenced anyway by the graph. Since we do
                        --  not have the real instruction, create a dummy one
                        --  so that a proper node is generated for it.

                        declare
                           PC         : constant Pc_Type := BP.Target.Address;
                           Dummy_Insn : Instruction_Access :=
                             new Instruction_Record'
                               (Bytes => (null, PC, No_PC),
                                Section => 0,
                                others => <>);
                           Cur        : Instruction_Sets.Cursor;
                           Inserted   : Boolean;
                        begin
                           Context.Other_Outcome.Insert
                             (Dummy_Insn, Cur, Inserted);
                           if not Inserted then
                              Free (Dummy_Insn);
                           end if;
                        end;
                     end if;
                  end if;
                  Insn.Control_Flow_Pair := BP.Branch_Instruction;
                  Insn.Successors.Append (BP.Target);

                  Branch_Points.Delete (Cur);
               end if;
               Cur := Next_Cur;
            end;
         end loop;

         --  Now, prepare the next iteration

         if Last_Insn /= null and then not Last_Insn_Added then
            Free (Last_Insn);
         end if;
         if Insn_Has_FT then
            Last_Insn := Insn;
            Last_Insn_Added := Insn_Added;
            Insn := null;
            Insn_Added := False;
         else
            Last_Insn := null;
            Last_Insn_Added := False;
         end if;

         --  Stop when wrapping

         exit when PC < Old_PC;
      end loop;
   end Collect_Instructions;

   ------------------------
   -- Build_Basic_Blocks --
   ------------------------

   procedure Build_Basic_Blocks (Context : Context_Access) is
      CFG : Basic_Blocks renames Context.CFG;
      Current_BB : Basic_Block_Access := null;

      procedure Finalize_Successors (BB : in out Basic_Block);
      --  Remove successors of all instructions in BB except for the last
      --  instruction. Such successors are useless since the only possible
      --  successor for such instructions is the next one. Also elaborate
      --  the "executed" flag. for successors.

      -------------------------
      -- Finalize_Successors --
      -------------------------

      procedure Finalize_Successors (BB : in out Basic_Block) is
         use Instruction_Vectors;
         use Successor_Vectors;
      begin
         for Cur in BB.Iterate loop
            if Cur /= BB.Last then
               Element (Cur).Successors.Clear;
            else
               --  By default, no successor has been reached

               for Succ of BB.Last_Element.Successors loop
                  Element (Cur).Executed_Successors.Append (False);
               end loop;
            end if;
         end loop;
      end Finalize_Successors;

   --  Start of processing for Build_Basic_Blocks

   begin
      for Insn of Context.Instructions loop
         if Current_BB = null
           or else Context.Basic_Block_Starters.Contains (Address (Insn))
         then
            if Current_BB /= null then
               Finalize_Successors (Current_BB.all);
            end if;
            Current_BB := new Basic_Block;
            CFG.Insert (Insn.Bytes.First, Current_BB);
         end if;
         Current_BB.Append (Insn);
      end loop;

      if Current_BB /= null then
         Finalize_Successors (Current_BB.all);
      end if;

      Context.Instructions.Clear;
   end Build_Basic_Blocks;

   ------------------------
   -- Clear_Basic_Blocks --
   ------------------------

   procedure Clear_Basic_Blocks (Context : Context_Access) is
   begin
      for BB of Context.CFG loop
         for Insn of BB.all loop
            Free (Insn);
         end loop;
         Free (BB);
      end loop;
   end Clear_Basic_Blocks;

   -------------------------------
   -- Tag_Executed_Instructions --
   -------------------------------

   procedure Tag_Executed_Instructions
     (Context           : Context_Access;
      Exec_Path         : String;
      Traces_Files_List : Inputs.Inputs_Type)
   is
      use type Interfaces.Unsigned_8;
      use Traces_Dbase;
      use Traces_Files;

      Base : Traces_Base;

      procedure Import_Traces (Filename : String);
      --  Import the trace from Filename into Base

      procedure Process_Trace_Entry (Trace : Trace_Entry);
      --  Tag as executed all the instructions in the CFG it covers

      procedure Process_Trace_Op
        (Insn : Instruction_Access; Op : Interfaces.Unsigned_8);
      --  Tag as executed all the successors in Insn

      -------------------
      -- Import_Traces --
      -------------------

      procedure Import_Traces (Filename : String) is
         Trace_File : Trace_File_Type;
      begin
         Read_Trace_File (Filename, Trace_File, Base);
         declare
            Mismatch_Reason : constant String :=
              Match_Trace_Executable (Context.Exec.all, Trace_File);

         begin
            if Mismatch_Reason /= "" then
               Warn
                 ("ELF file " & Exec_Path
                  & " does not seem to match trace file "
                  & Filename & ": " & Mismatch_Reason);
            end if;
         end;
      end Import_Traces;

      -------------------------
      -- Process_Trace_Entry --
      -------------------------

      procedure Process_Trace_Entry (Trace : Trace_Entry) is
         use Basic_Block_Maps;
         BB          : Cursor;
         Branch_Insn : Instruction_Access;
      begin
         --  Look for the last basic block that starts before this trace entry

         BB := Context.CFG.Floor (Trace.First);

         --  If we cannot find any, the first selected basic block may start
         --  after this trace entry so give it another try. Note that the CFG
         --  is supposed to contain at least one basic block, so First_Element
         --  is safe.

         if BB = No_Element
           and then Address (Context.CFG.First_Element.all) <= Trace.Last
         then
            BB := Context.CFG.First;
         end if;

         while BB /= No_Element
           and then Address (Element (BB).all) <= Trace.Last
         loop
            --  Tag executed instructions

            for Insn of Element (BB).all loop
               --  Tag executed instructions

               if Address (Insn) in Trace.First .. Trace.Last then
                  Insn.Executed := True;
               end if;

               --  Tag reached successors.

               if Insn.Bytes.Last = Trace.Last then
                  if Insn.Control_Flow_Pair /= Address (Insn) then

                     --  On architectures with delay slot, the "branch taken"
                     --  information may be present on the branch instruction
                     --  instead of on the instruction on which the branch
                     --  applies. Due to the nature of delay slots, this branch
                     --  instruction may appear in the middle of a basic block.

                     Branch_Insn := Get_Instruction
                       (Context.CFG, Insn.Control_Flow_Pair);

                  else
                     Branch_Insn := Insn;
                  end if;

                  if Branch_Insn /= null then
                     Process_Trace_Op (Branch_Insn, Trace.Op);
                  end if;
               end if;
            end loop;

            declare
               use Ada.Containers;

               Last_Insn : constant Instruction_Access :=
                 Element (BB).Last_Element;
            begin
               if Last_Insn.Bytes.First in Trace.First .. Trace.Last
                 and then Last_Insn.Successors.Length = 1
               then
                  --  This instruction ends this basic block, but appears
                  --  in the middle of a trace entry: this can only be a
                  --  non-branch instruction, and thus it should have excactly
                  --  one successor. Ignore when there is more than one
                  --  successor anyway: emulators may start a trace entry
                  --  with a delay-slot instruction.

                  Last_Insn.Executed_Successors.Replace_Element
                    (Last_Insn.Executed_Successors.First_Index,
                     Last_Insn.Executed);
               end if;
            end;
            Next (BB);
         end loop;
      end Process_Trace_Entry;

      ----------------------
      -- Process_Trace_Op --
      ----------------------

      procedure Process_Trace_Op
        (Insn : Instruction_Access; Op : Interfaces.Unsigned_8)
      is
         use Ada.Containers;
         use Qemu_Traces;

         I         : Natural;
      begin
         I := Insn.Successors.First_Index;
         for Succ of Insn.Successors loop
            --  No computation is needed if this sucessor has already
            --  been proved as being reached.

            if not Insn.Executed_Successors.Element (I) then
               Insn.Executed_Successors.Replace_Element
                 (I,
                  ((Op and
                     (case Succ.Kind is
                         when Fallthrough =>
                            Trace_Op_Br1,
                         when Branch      =>
                      --  For unconditional branches, Op_Block is
                      --  used instead of Op_Br*.

                        (if Insn.Successors.Length = 1
                         then Trace_Op_Block
                         else Trace_Op_Br0),

                         when Subp_Return =>
                            Trace_Op_Block,

                         when Raise_Exception =>
                            Trace_Op_Fault))
                   /= 0));
            end if;
            I := I + 1;
         end loop;
      end Process_Trace_Op;

      Iter  : Entry_Iterator;
      Trace : Trace_Entry;

   --  Start of processingr for Tag_Executed_Instructions

   begin
      Init_Base (Base);

      --  Import all trace entries to the database, so that they are sorted by
      --  address

      Inputs.Iterate (Traces_Files_List, Import_Traces'Access);

      --  And then, use them to tag instructions

      Init (Base, Iter, Context.CFG.First_Key);
      loop
         Get_Next_Trace (Trace, Iter);
         exit when Trace = Bad_Trace;
         Process_Trace_Entry (Trace);
      end loop;
   end Tag_Executed_Instructions;

   ----------------
   -- Output_CFG --
   ----------------

   procedure Output_CFG (Context : Context_Access) is
      use Ada.Strings.Unbounded;
      use type SC_Obligations.SCO_Id;
      subtype SCO_Id is SC_Obligations.SCO_Id;

      F : Output_Type renames Context.Output;
      Unknown_Address_Counter : Natural := 0;

      function "=" (Left, Right : Basic_Block_Maps.Map) return Boolean is
        (False);
      package Grouped_Basic_Blocks_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => SCO_Id,
         Element_Type => Basic_Block_Maps.Map);
      Grouped_Basic_Blocks : Grouped_Basic_Blocks_Maps.Map;
      --  When decision static analysis has been performed, this map is used
      --  to group basic blocks by decision, so that they are displayed under
      --  their condition's cluster.

      type Cond_Edge_Info is record
         Branch_Info : Decision_Map.Cond_Branch_Maps.Cursor;
         Edge        : Decision_Map.Edge_Kind;
      end record;
      --  Gathers pointers to information about a edge and the corresponding
      --  conditional branch.

      No_Cond_Edge : constant Cond_Edge_Info :=
        (Decision_Map.Cond_Branch_Maps.No_Element,
         Decision_Map.Fallthrough);

      type Edge_Descriptor is record
         From_Id, To_Id     : Unbounded_String;
         Kind               : Successor_Kind;
         Selected, Executed : Boolean;
         Info               : Cond_Edge_Info;
      end record;
      --  Gather information needed to output an edge. In the dot output, edges
      --  must be out of any subgraph. This is used to bufferize all edges when
      --  visiting basic blocks so that they are all output at the end of the
      --  dot document.

      package Edge_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Edge_Descriptor);
      Edges : Edge_Lists.List;

      function First_Digit (Address : String) return Natural;
      --  Return the index of the first non-null digit in Address, or
      --  Address'Last if it contains only zeros.

      function Colored
        (Text : String; Color : Highlighting.Color_Type) return String;
      --  Return markup code for Text to be displayed with Color

      function Styled
        (Text  : String; Style : Highlighting.Token_Style_Type) return String;
      --  Return markup code for Text to be displayed according so Style

      function Hex_Colored_Image
        (Address : Pc_Type; Color : Highlighting.Color_Type) return String;
      --  Return a colored hexadecimal string

      function Tristate_Colored_Image
        (T : SC_Obligations.Tristate) return String is
        (case T is
            when SC_Obligations.Unknown => Colored ("???", Unknown_Color),
            when SC_Obligations.True    => Colored ("True", True_Color),
            when SC_Obligations.False   => Colored ("False", False_Color));

      function Node_Id (Address : Pc_Type) return String;
      --  Return the dot identifier for some node

      function Get_Unknown_Address_Id return String;
      --  Return an unique dot identifier for some unknown address. It is up to
      --  the caller to create such node.

      function HTML_Escape (S : String) return String;
      --  Escape HTML special characters into HTML entities

      function Format_Slocs_For_PC
        (PC        : Pc_Type;
         Last_Sloc : Address_Info_Sets.Cursor) return String;
      --  Format the list of slocs associated to the instruction at PC for a
      --  dot label.

      function Format_Slocs_For_PC (PC : Pc_Type) return String;
      --  Likewise, but automatically look for the sloc cursor

      function Format_Basic_Block_Label
        (BB : Basic_Block) return String;
      --  Format the content of a basic block (i.e. its instructions) into a
      --  suitable dot label.

      function Get_Cond_Edge_Info
        (From_Insn : Instruction_Access;
         Successor : Successor_Record) return Cond_Edge_Info;
      --  Search and return information about an edge and its conditional
      --  branch, if any. Return No_Cond_Edge otherwise.

      function Get_Cond_Branch_Info
        (From_BB   : Basic_Block) return Decision_Map.Cond_Branch_Maps.Cursor
      is
        (Decision_Map.Cond_Branch_Map.Find
           ((Context.Exec, From_BB.Last_Element.Control_Flow_Pair)));

      procedure Group_BB_By_Condition;

      procedure Output_Edge (Edge : Edge_Descriptor);

      procedure Add_Basic_Block
        (BB : Basic_Block_Access);

      -----------------
      -- First_Digit --
      -----------------

      function First_Digit (Address : String) return Natural is
      begin
         for I in Address'Range loop
            if Address (I) /= '0' then
               return I;
            end if;
         end loop;
         return Address'Last;
      end First_Digit;

      Longest_Address       : constant String :=
        Hex_Image (Address (Context.CFG.Last_Element.Last_Element));
      First_Addresses_Digit : constant Natural :=
        First_Digit (Longest_Address);

      -------------
      -- Colored --
      -------------

      function Colored
        (Text : String; Color : Highlighting.Color_Type) return String is
      begin
         return Styled (Text, (Color, False, False, False));
      end Colored;

      ------------
      -- Styled --
      ------------

      function Styled
        (Text  : String; Style : Highlighting.Token_Style_Type) return String
      is
         Prefix, Suffix : Unbounded_String;
      begin
         Append (Prefix, "<FONT COLOR=""#" & Style.Color & """>");
         Insert (Suffix, 1, "</FONT>");
         if Style.Bold then
            Append (Prefix, "<B>");
            Insert (Suffix, 1, "</B>");
         end if;
         if Style.Italic then
            Append (Prefix, "<I>");
            Insert (Suffix, 1, "</I>");
         end if;
         if Style.Underlined then
            Append (Prefix, "<U>");
            Insert (Suffix, 1, "</U>");
         end if;
         return To_String (Prefix) & Text & To_String (Suffix);
      end Styled;

      -----------------------
      -- Hex_Colored_Image --
      -----------------------

      function Hex_Colored_Image
        (Address : Pc_Type; Color : Highlighting.Color_Type) return String
      is
         S      : constant String := Hex_Image (Address);
         Result : constant String := S (First_Addresses_Digit .. S'Last);
      begin
         return Colored (Result, Color);
      end Hex_Colored_Image;

      -------------
      -- Node_Id --
      -------------

      function Node_Id (Address : Pc_Type) return String is
        ("bb_" & Hex_Image (Address));

      ----------------------------
      -- Get_Unknown_Address_Id --
      ----------------------------

      function Get_Unknown_Address_Id return String is
      begin
         Unknown_Address_Counter := Unknown_Address_Counter + 1;
         return "unknown_" & Hex_Image (Pc_Type (Unknown_Address_Counter));
      end Get_Unknown_Address_Id;

      -----------------
      -- HTML_Escape --
      -----------------

      function HTML_Escape (S : String) return String is
         Result : Unbounded_String;
      begin
         for C of S loop
            case C is
               when '&' =>
                  Append (Result, "&amp;");
               when '<' =>
                  Append (Result, "&lt;");
               when '>' =>
                  Append (Result, "&gt;");

               --  These two entities are not necessary according to the dot
               --  language specification, this is a workaround for a bug in
               --  the graphviz/dot parser.

               when '[' =>
                  Append (Result, "&#91;");
               when ']' =>
                  Append (Result, "&#93;");

               when others =>
                  Append (Result, C);
            end case;
         end loop;
         return To_String (Result);
      end HTML_Escape;

      -------------------------
      -- Format_Slocs_For_PC --
      -------------------------

      function Format_Slocs_For_PC
        (PC        : Pc_Type;
         Last_Sloc : Address_Info_Sets.Cursor) return String
      is
         use Address_Info_Sets;

         Sloc   : Cursor := Last_Sloc;
         Result : Unbounded_String;
      begin
         while Sloc /= No_Element
           and then (PC in Element (Sloc).First .. Element (Sloc).Last)
         loop
            Append
              (Result,
               Colored
                 (HTML_Escape (Object_Locations.Image (Element (Sloc))),
                  Sloc_Color));
            Append (Result, "<BR ALIGN=""left""/>");
            Previous (Sloc);
         end loop;
         return To_String (Result);
      end Format_Slocs_For_PC;

      -------------------------
      -- Format_Slocs_For_PC --
      -------------------------

      function Format_Slocs_For_PC (PC : Pc_Type) return String
      is
         use Address_Info_Sets;

         Subp : constant Address_Info_Acc := Get_Address_Info
           (Context.Exec.all, Subprogram_Addresses, PC);
         Sloc : Cursor := No_Element;
      begin
         if Subp /= null then
            Sloc := Find_Address_Info
              (Subp.Lines, Line_Addresses, PC);
         end if;
         return Format_Slocs_For_PC (PC, Sloc);
      end Format_Slocs_For_PC;

      ------------------------------
      -- Format_Basic_Block_Label --
      ------------------------------

      function Format_Basic_Block_Label
        (BB : Basic_Block) return String
      is
         use Address_Info_Sets;

         Result : Unbounded_String;

         Disas    : access Disassemblers.Disassembler'Class;
         Buffer   : Highlighting.Buffer_Type (128);
         Insn_Len : Natural;

         Branch                : Branch_Kind;
         Flag_Indir, Flag_Cond : Boolean;
         Branch_Dest, FT_Dest  : Disassemblers.Dest;

         Last_Sloc : Cursor := Address_Info_Sets.No_Element;
         Sloc      : Cursor;

         Subp : constant Address_Info_Acc := Get_Address_Info
           (Context.Exec.all, Subprogram_Addresses, Address (BB));

      begin
         for Insn of BB loop
            Disas := Elf_Disassemblers.Disa_For_Machine
              (Traces.Machine, Insn.Insn_Set);

            if Subp /= null then
               Sloc := Find_Address_Info
                 (Subp.Lines, Line_Addresses, Address (Insn));
            end if;
            if Sloc /= Last_Sloc then
               Last_Sloc := Sloc;
               Append (Result, Format_Slocs_For_PC (Address (Insn), Sloc));
            end if;

            Buffer.Reset;
            Disas.Disassemble_Insn_Or_Abort
              (Insn.Bytes, Address (Insn),
               Buffer,
               Insn_Len,
               Context.Exec.all);
            Disas.Get_Insn_Properties
              (Insn.Bytes, Address (Insn),
               Branch,
               Flag_Indir, Flag_Cond,
               Branch_Dest, FT_Dest);
            Append (Result, "  ");
            Append
              (Result,
               Hex_Colored_Image
                 (Address (Insn),
                  (if Context.Tag_Executed
                   then
                     (if Insn.Executed
                      then Executed_Insn_Color
                      else Unexecuted_Insn_Color)
                  else Hex_Color)));
            Append (Result, "  ");
            declare
               use Highlighting;

               Cur : Highlighting.Cursor := Buffer.First;
               Mnemonic_Kind : constant Token_Kind :=
                 (case Branch is
                     when Br_Call | Br_Ret =>
                    (if Flag_Cond
                     then Mnemonic_Branch
                     else Mnemonic_Call),

                     when Br_Jmp =>
                    (if Flag_Cond
                     then Mnemonic_Branch
                     else Mnemonic_Call),

                     when others => Mnemonic);
               Kind : Token_Kind;

            begin
               while Cur /= Highlighting.No_Element loop
                  Kind := Token (Cur);
                  if Kind = Mnemonic then
                     Kind := Mnemonic_Kind;
                  end if;
                  Append
                    (Result,
                     Styled (HTML_Escape (Text (Cur)), Style_Default (Kind)));
                  Next (Cur);
               end loop;
            end;
            Append (Result, "<BR ALIGN=""left""/>");
         end loop;
         return To_String (Result);
      end Format_Basic_Block_Label;

      ------------------------
      -- Get_Cond_Edge_Info --
      ------------------------

      function Get_Cond_Edge_Info
        (From_Insn : Instruction_Access;
         Successor : Successor_Record) return Cond_Edge_Info
      is
         use Decision_Map;
         use Decision_Map.Cond_Branch_Maps;

         Cond_Branch : constant Cursor := Decision_Map.Cond_Branch_Map.Find
           ((Context.Exec, From_Insn.Control_Flow_Pair));
      begin
         if Cond_Branch /= No_Element then
            case Successor.Kind is
               when Fallthrough =>
                  return (Cond_Branch, Fallthrough);
               when Branch =>
                  return (Cond_Branch, Branch);
               when others =>
                  null;
            end case;
         end if;
         return No_Cond_Edge;
      end Get_Cond_Edge_Info;

      ---------------------------
      -- Group_BB_By_Condition --
      ---------------------------

      procedure Group_BB_By_Condition is
         use type Ada.Containers.Count_Type;
         use Basic_Block_Maps;
         use Decision_Map.Cond_Branch_Maps;
         use SC_Obligations;

         Visited_BBs : PC_Sets.Set;
         BB          : Basic_Block_Access;
         Branch_Info : Decision_Map.Cond_Branch_Maps.Cursor;
         Condition   : SCO_Id;
      begin
         for Cur in Context.CFG.Iterate loop

            --  Look for the condition this basic block belongs to. Follow
            --  successors when they are unique (i.e. fallthroughs and
            --  unconditional jumps).

            BB := Element (Cur);
            Condition := No_SCO_Id;
            Visited_BBs.Clear;
            loop
               --  If we already met this BB: do not loop infinitely. Just
               --  admit we will not know the condition this BB belongs to.

               exit when Visited_BBs.Contains (Address (BB.all));
               Visited_BBs.Insert (Address (BB.all));

               Branch_Info := Get_Cond_Branch_Info (BB.all);
               if Branch_Info /= Decision_Map.Cond_Branch_Maps.No_Element then
                  Condition := Reference
                    (Decision_Map.Cond_Branch_Map, Branch_Info).Condition;
                  exit;

               elsif Successors (BB.all).Length = 1
                 and then (Successors (BB.all).First_Element.Kind in
                               Fallthrough | Branch)
               then
                  declare
                     Successor : Successor_Record renames
                       Successors (BB.all).First_Element;
                     Next_BB   : constant Basic_Block_Maps.Cursor :=
                       Context.CFG.Find
                         (if Successor.Known
                          then Successor.Address
                          else No_PC);
                  begin
                     if Next_BB /= Basic_Block_Maps.No_Element then
                        BB := Element (Next_BB);
                     else
                        exit;
                     end if;
                  end;

               else
                  exit;
               end if;
            end loop;

            if not Grouped_Basic_Blocks.Contains (Condition) then
               Grouped_Basic_Blocks.Insert
                 (Condition, Basic_Block_Maps.Empty_Map);
            end if;
            Grouped_Basic_Blocks.Reference (Condition).Insert
              (Key (Cur), Element (Cur));
         end loop;
      end Group_BB_By_Condition;

      --------------
      -- Output_Edge --
      --------------

      procedure Output_Edge (Edge : Edge_Descriptor)
      is
         Color : constant Highlighting.Color_Type :=
           (if Edge.Selected
            then
              (if Edge.Executed
               then Edge_Executed_Color
               else Edge_Default_Color)
            else Edge_Unselected_Color);
      begin
         Put (F, To_String (Edge.From_Id));
         Put (F, " -> ");
         Put (F, To_String (Edge.To_Id));
         Put (F, " [color=""#" & Color & """,penwidth=3,style=");
         Put (F, (case Edge.Kind is
                 when Fallthrough                   => "solid",
                 when Branch                        => "dashed",
                 when Subp_Return | Raise_Exception => "dotted"));

         --  If possible, annotate this edge with decision static analysis
         --  results.

         if Edge.Info /= No_Cond_Edge then
            declare
               use Decision_Map;
               use Decision_Map.Cond_Branch_Maps;
               use SC_Obligations;
               use Strings;

               This_Branch  : Decision_Map.Cond_Branch_Info renames
                 Reference (Cond_Branch_Map, Edge.Info.Branch_Info);
               This_Edge    : Decision_Map.Cond_Edge_Info renames
                 This_Branch.Edges (Edge.Info.Edge);
               Line_Started : Boolean := False;
            begin
               Put (F, ",label=<");
               if This_Edge.Origin /= Unknown then
                  Put (F, "Cond #");
                  Put (F, Img (Integer (This_Branch.Condition)));
                  Put (F, ' ');
                  Put (F, Tristate_Colored_Image (This_Edge.Origin));
                  Line_Started := True;
               end if;

               case This_Edge.Dest_Kind is
                  when Outcome =>
                     if Line_Started then
                        Put (F, "<BR />");
                     end if;
                     Put (F, "Decision #");
                     Put (F, Img (Integer (Enclosing_Decision
                          (This_Branch.Condition))));
                     Put (F, " is ");
                     Put (F, Tristate_Colored_Image (This_Edge.Outcome));
                     if Degraded_Origins
                          (Enclosing_Decision (This_Branch.Condition))
                     then
                        Put (F, "???");
                     end if;
                     Line_Started := True;

                  when Raise_Exception =>
                     if Line_Started then
                        Put (F, "<BR />");
                     end if;
                     Put (F, "Exception");
                     Line_Started := True;

                  when Unknown =>
                     if Line_Started then
                        Put (F, "<BR />");
                     end if;
                     Put (F, "???");
                     Line_Started := True;

                  when others =>
                     null;
               end case;
               Put (F, '>');
            end;
         end if;

         Put_Line (F, "];");
      end Output_Edge;

      ---------------------
      -- Add_Basic_Block --
      ---------------------

      procedure Add_Basic_Block
        (BB : Basic_Block_Access)
      is
         use Boolean_Vectors;
         use type Ada.Containers.Count_Type;

         Succ_Cur : Cursor;
      begin
         Put (F, Node_Id (Address (BB.all)));
         Put (F, " [shape=box, fontname=monospace, label=<");
         Put (F, Format_Basic_Block_Label (BB.all));
         Put_Line (F, ">];");

         Succ_Cur := BB.Last_Element.Executed_Successors.First;
         for Successor of Successors (BB.all) loop
            --  The only successor for an unconditional return instruction is
            --  obvious. Strip it to reduce noise.

            if (Successor.Kind /= Subp_Return
                and then Successor.Kind /= Raise_Exception)
              or else Successors (BB.all).Length > 1
            then
               declare
                  Id            : constant String :=
                    (if Successor.Known
                     then Node_Id (Successor.Address)
                     else Get_Unknown_Address_Id);
                  Successor_Key : aliased Instruction_Record :=
                    (Bytes  => (First => Successor.Address, others => <>),
                     others => <>);
                  Edge_Info     : constant Cond_Edge_Info :=
                    Get_Cond_Edge_Info (BB.Last_Element, Successor);
               begin
                  Edges.Append
                    ((To_Unbounded_String (Node_Id (Address (BB.all))),
                     To_Unbounded_String (Id),
                     Successor.Kind,
                     Selected =>
                       (not Successor.Known
                        or else not Context.Other_Outcome.Contains
                          (Successor_Key'Unchecked_Access)),
                     Executed => Element (Succ_Cur),
                     Info     => Edge_Info));
                  if not Successor.Known then
                     Put_Line (F, Id & " [shape=ellipse, label=<???>];");
                  end if;
               end;
            end if;

            Next (Succ_Cur);
         end loop;
      end Add_Basic_Block;

   --  Start of processing for Output_CFG

   begin
      Put_Line (F, "digraph cfg {");

      if Context.Group_By_Condition then
         Group_BB_By_Condition;
         for Group in Grouped_Basic_Blocks.Iterate loop
            declare
               use Grouped_Basic_Blocks_Maps;

               Unknown_Cond : constant Boolean :=
                 Key (Group) = SC_Obligations.No_SCO_Id;
            begin
               if not Unknown_Cond then
                  Put (F, "subgraph cluster_condition_");
                  Put (F, Strings.Img (Integer (Key (Group))));
                  Put_Line (F, " {");
               end if;
               for BB of Grouped_Basic_Blocks.Reference (Group) loop
                  Add_Basic_Block (BB);
               end loop;
               if not Unknown_Cond then
                  Put_Line (F, "}");
               end if;
            end;
         end loop;

      else
         for BB of Context.CFG loop
            Add_Basic_Block (BB);
         end loop;
      end if;

      declare
         procedure Output_Unknown_Nodes (Insns : Instructions_Set);

         --------------------------
         -- Output_Unknown_Nodes --
         --------------------------

         procedure Output_Unknown_Nodes (Insns : Instructions_Set) is
         begin
            for Insn of Insns loop
               Put (F, Node_Id (Address (Insn)));
               Put (F, " [shape=ellipse, fontname=monospace, label=<");
               Put (F, Format_Slocs_For_PC (Address (Insn)));
               Put (F, "0x" & Hex_Colored_Image (Address (Insn), Hex_Color));
               Put_Line (F, ">];");
            end loop;
         end Output_Unknown_Nodes;

      begin
         Output_Unknown_Nodes (Context.Other_Outcome);
         Output_Unknown_Nodes (Context.Other_Income);
         for Insn of Context.Other_Income loop
            for Successor of Insn.Successors loop
               --  We are not interested in unknown successors of unselected
               --  instructions.

               if Successor.Known
                 and then Context.CFG.Contains (Successor.Address)
               then
                  declare
                     Edge_Info : constant Cond_Edge_Info :=
                       Get_Cond_Edge_Info (Insn, Successor);
                  begin
                     Edges.Append
                       ((To_Unbounded_String (Node_Id (Address (Insn))),
                        To_Unbounded_String (Node_Id (Successor.Address)),
                        Successor.Kind,
                        Selected => False,
                        Executed => False,
                        Info     => Edge_Info));
                  end;
               end if;
            end loop;
         end loop;
      end;

      for Edge of Edges loop
         Output_Edge (Edge);
      end loop;

      Put_Line (F, "}");
   end Output_CFG;

   ----------
   -- Dump --
   ----------

   procedure Dump (Exec_Path         : String;
                   Locations         : User_Locations;
                   Output            : String_Access;
                   Format            : Output_Format;
                   SCO_Files_List    : Inputs.Inputs_Type;
                   Traces_Files_List : Inputs.Inputs_Type;
                   Keep_Edges        : Boolean)
   is
      use Ada.Containers;

      Context           : aliased Context_Type;
      Ctx               : constant Context_Access :=
        Context'Unrestricted_Access;
      Output_File       : aliased File_Type;
   begin
      Context.Group_By_Condition := Inputs.Length (SCO_Files_List) > 0;
      Context.Keep_Edges := Keep_Edges;
      Context.Tag_Executed := Inputs.Length (Traces_Files_List) > 0;

      if Switches.Verbose then
         Report
           (Msg  => "Hello, this is the dumper!",
            Kind => Notice);
         Report
           (Msg  => "Dumping code from: " & Exec_Path,
            Kind => Notice);
         Report
           (Msg  =>
              (if Output = null
               then "To standard output"
               else "To " & Output.all),
            Kind => Notice);
         Report
           (Msg  => "Format: " & Output_Format'Image (Format),
            Kind => Notice);
         Report
           (Msg  =>
              (if Keep_Edges
               then "Keep edges that follow exception raises"
               else "Strip edges that follow exception raises"),
            Kind => Notice);
      end if;

      begin
         Execs_Dbase.Open_Exec (Exec_Path, 0, Context.Exec);
      exception
         when Binary_Files.Error =>
            Fatal_Error ("Could not open " & Exec_Path);
      end;

      if Switches.Verbose then
         Report (Msg => "Reading symbols...", Kind => Notice);
      end if;
      Build_Symbols (Context.Exec.all);

      Translate_Locations (Ctx.Exec, Locations, Context.Locs);

      if Context.Group_By_Condition then
         Coverage.Set_Coverage_Levels ("stmt+mcdc");
         if Switches.Verbose then
            Report (Msg => "Loading ALI files...", Kind => Notice);
         end if;
         Inputs.Iterate (SCO_Files_List, SC_Obligations.Load_SCOs'Access);
         Coverage.Source.Initialize_SCI;

         if Switches.Verbose then
            Report (Msg => "Reading routine names...", Kind => Notice);
         end if;
         Read_Routine_Names (Context.Exec.all, Exclude => False);
      end if;

      if Switches.Verbose then
         Report (Msg => "Reading debug line info...", Kind => Notice);
      end if;
      Build_Debug_Lines (Context.Exec.all);

      if Context.Group_By_Condition then
         if Switches.Verbose then
            Report
              (Msg => "Performing static analysis for decisions...",
               Kind => Notice);
         end if;
         Decision_Map.Analyze (Context.Exec);
      end if;

      declare
         Section          : Address_Info_Acc;
         Section_Iterator : Addresses_Iterator;
      begin
         Init_Iterator (Context.Exec.all, Section_Addresses, Section_Iterator);
         loop
            Next_Iterator (Section_Iterator, Section);
            exit when Section = null;

            if Section.Section_Name.all = ".text" then
               if Switches.Verbose then
                  Report
                    (Msg => "ELF section #"
                     & Strings.Img (Integer (Section.Section_Sec_Idx))
                     & " looks interesting: loading its instructions...",
                     Kind => Notice);
               end if;
               Load_Section_Content (Context.Exec.all, Section);
               Collect_Instructions (Ctx, Section);
            end if;
         end loop;
      end;

      if Ctx.Instructions.Is_Empty then
         Report
           (Msg  => "No code matched: the graph is empty",
            Kind => Warning);
         return;
      end if;

      --  Prepare the output stream

      if Format = None then
         Context.Output.Kind := File;
         if Output = null then
            declare
               function Convert is new Ada.Unchecked_Conversion
                 (File_Access, Output_File_Access);
            begin
               Context.Output.File := Convert (Standard_Output);
               Context.Output.To_Close := False;
            end;
         else
            Create (File => Output_File, Name => Output.all);
            Context.Output.File := Output_File'Unchecked_Access;
            Context.Output.To_Close := True;
         end if;

      else
         declare
            Format_Flag : constant String :=
              (case Format is
                  when None => raise Program_Error,
                  when Dot  => "dot",
                  when SVG  => "svg",
                  when PDF  => "pdf",
                  when PNG  => "png");
            Output_Args : constant GNAT.OS_Lib.Argument_List :=
              (if Output = null then
                 (1 .. 0 => <>)
               else
                 (new String'("-o"), new String'(Output.all)));
            Args        : constant GNAT.OS_Lib.Argument_List :=
              (1 => new String'("-T" & Format_Flag)) & Output_Args;
         begin
            Context.Output.Kind := Subprocess;
            Non_Blocking_Spawn (Context.Output.Pd, "dot", Args);
         end;
      end if;

      Build_Basic_Blocks (Ctx);
      Tag_Executed_Instructions (Ctx, Exec_Path, Traces_Files_List);
      Output_CFG (Ctx);
      Clear_Basic_Blocks (Ctx);

      begin
         Close (Context.Output);
      exception
         when Error : Program_Error =>
            Fatal_Error
              ("call to dot failed: " & Exception_Information (Error));
      end;
   end Dump;

   ---------
   -- Put --
   ---------

   procedure Put (Output : in out Output_Type; C : Character) is
   begin
      case Output.Kind is
         when File =>
            Put (Output.File.all, C);
         when Subprocess =>
            Send (Output.Pd, (1 => C), Add_LF => False);
      end case;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Output : in out Output_Type; S : String) is
   begin
      case Output.Kind is
         when File =>
            Put (Output.File.all, S);
         when Subprocess =>
            Send (Output.Pd, S, Add_LF => False);
      end case;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Output : in out Output_Type; S : String) is
   begin
      case Output.Kind is
         when File =>
            Put_Line (Output.File.all, S);
         when Subprocess =>
            Send (Output.Pd, S, Add_LF => True);
      end case;
   end Put_Line;

   -----------
   -- Close --
   -----------

   procedure Close (Output : in out Output_Type) is
   begin
      if Output.To_Close then
         case Output.Kind is
            when File =>
               Close (Output.File.all);
            when Subprocess =>
               declare
                  Status : Integer;
               begin
                  Close (Output.Pd, Status);
                  if Status /= 0 then
                     raise Program_Error with
                       ("subprocess exited with status code"
                        & Integer'Image (Status));
                  end if;
               end;
         end case;
         Output.To_Close := False;
      end if;
   end Close;

end CFG_Dump;
