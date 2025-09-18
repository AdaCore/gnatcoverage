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

with Ada.Command_Line;
with Ada.Containers;

with Support_Files; use Support_Files;
with Version;       use Version;

package body Coverage is

   Levels : Levels_Type := (others => False);
   --  Global variable that records the coverage operation that has been asked
   --  to xcov. Set using Set_Coverage_Levels.

   --  Maps from valid --level string values to internal Levels array,
   --  such as:
   --
   --  "stmt" -> (stmt => True, others => False);
   --  "stmt+decision" -> (stmt => True, Decision => True, others => False)
   --  ...
   --
   --  One map per family of criteria (source vs object coverage).

   Object_Coverage_Enabled_Cached : Boolean;
   Source_Coverage_Enabled_Cached : Boolean;
   MCDC_Coverage_Enabled_Cached   : Boolean;
   --  Global variables to hold cached return values for enabled coverage
   --  levels query functions. These functions can be called very often, so
   --  just returning a boolean removes any overhead. These globals are updated
   --  at each call of Set_Coverage_Levels, which is not called very often.

   function Any_Coverage_Enabled (L : Levels_Type) return Boolean;
   --  True if any level marked True in L is enabled

   ---------------------------
   -- Coverage_Option_Value --
   ---------------------------

   function Coverage_Option_Value return String is
   begin
      return Coverage_Option_Value (Levels);
   end Coverage_Option_Value;

   -------------
   -- Enabled --
   -------------

   function Enabled (Level : Coverage_Level) return Boolean is
   begin
      return Levels (Level);
   end Enabled;

   --------------------------
   -- Any_Coverage_Enabled --
   --------------------------

   function Any_Coverage_Enabled (L : Levels_Type) return Boolean is
   begin
      return (L and Levels) /= Levels_Type'(others => False);
   end Any_Coverage_Enabled;

   -------------------------------
   -- Decision_Coverage_Enabled --
   -------------------------------

   function Decision_Coverage_Enabled return Boolean is
   begin
      return MCDC_Coverage_Enabled or else Enabled (Decision);
   end Decision_Coverage_Enabled;

   ---------------------------
   -- MCDC_Coverage_Enabled --
   ---------------------------

   function MCDC_Coverage_Enabled return Boolean is
   begin
      return MCDC_Coverage_Enabled_Cached;
   end MCDC_Coverage_Enabled;

   --------------------------------
   -- Assertion_Coverage_Enabled --
   --------------------------------

   function Assertion_Coverage_Enabled return Boolean is
   begin
      return
        Source_Coverage_Enabled
        and then (Enabled (ATC) or else Enabled (ATCC));
   end Assertion_Coverage_Enabled;

   ------------------------------------------
   -- Assertion_Condition_Coverage_Enabled --
   ------------------------------------------

   function Assertion_Condition_Coverage_Enabled return Boolean
   is (Enabled (ATCC));

   ----------------
   -- MCDC_Level --
   ----------------

   function MCDC_Level return MCDC_Coverage_Level is
   begin
      pragma Assert (MCDC_Coverage_Enabled);
      if Enabled (UC_MCDC) then
         pragma Assert (not Enabled (MCDC));
         return UC_MCDC;
      else
         pragma Assert (not Enabled (UC_MCDC));
         return MCDC;
      end if;
   end MCDC_Level;

   -------------------------------
   -- Assertion_Condition_Level --
   -------------------------------

   function Assertion_Condition_Level return Contract_Condition_Level is
   begin
      pragma Assert (Assertion_Condition_Coverage_Enabled);
      pragma Assert (Enabled (ATCC));
      return ATCC;
   end Assertion_Condition_Level;

   ----------------
   -- Object_Level --
   ----------------

   function Object_Level return Object_Coverage_Level is
   begin
      pragma Assert (Object_Coverage_Enabled);
      if Enabled (Insn) then
         pragma Assert (not Enabled (Branch));
         return Insn;
      else
         pragma Assert (not Enabled (Insn));
         return Branch;
      end if;
   end Object_Level;

   -----------------------------
   -- Object_Coverage_Enabled --
   -----------------------------

   function Object_Coverage_Enabled return Boolean is
   begin
      return Object_Coverage_Enabled_Cached;
   end Object_Coverage_Enabled;

   -----------------------------
   -- Source_Coverage_Enabled --
   -----------------------------

   function Source_Coverage_Enabled return Boolean is
   begin
      return Source_Coverage_Enabled_Cached;
   end Source_Coverage_Enabled;

   -------------------------
   -- Set_Coverage_Levels --
   -------------------------

   procedure Set_Coverage_Levels (Opt : String) is
      use Levels_Option_Maps;
      Cur : Cursor;
   begin
      --  Try to match a source level first, more likely

      Cur := Source_Levels_Option_Map.Find (Opt'Unrestricted_Access);

      if Cur = No_Element then
         Cur := Object_Levels_Option_Map.Find (Opt'Unrestricted_Access);
      end if;

      Levels := Element (Cur);

      Object_Coverage_Enabled_Cached :=
        Any_Coverage_Enabled
          ((Object_Coverage_Level => True, others => False));
      Source_Coverage_Enabled_Cached :=
        Any_Coverage_Enabled
          ((Source_Coverage_Level => True, others => False));
      MCDC_Coverage_Enabled_Cached :=
        Any_Coverage_Enabled ((MCDC_Coverage_Level => True, others => False));
   end Set_Coverage_Levels;

   --------------------
   -- Current_Levels --
   --------------------

   function Current_Levels return Levels_Type is
   begin
      return Levels;
   end Current_Levels;

   function Source_Levels_Enabled return Levels_Sets.Set is
      Res : Levels_Sets.Set;
   begin
      if Source_Coverage_Enabled then
         Res.Include (Stmt);
         if Decision_Coverage_Enabled then
            Res.Include (Decision);
            if MCDC_Coverage_Enabled then
               Res.Include (MCDC_Level);
            end if;
         end if;
         if Assertion_Coverage_Enabled then
            Res.Include (ATC);
            if Enabled (ATCC) then
               Res.Include (ATCC);
            end if;
         end if;
         if Enabled (Fun_Call) then
            Res.Include (Fun_Call);
         end if;
         if Enabled (GExpr) then
            Res.Include (GExpr);
         end if;
      end if;
      return Res;
   end Source_Levels_Enabled;

   -----------------------------
   -- Coverage_Levels_Enabled --
   -----------------------------

   function Coverage_Levels_Enabled return Levels_Sets.Set is
   begin
      if Source_Coverage_Enabled then
         return Source_Levels_Enabled;
      else
         declare
            Res : Levels_Sets.Set;
         begin
            if Enabled (Insn) then
               Res.Include (Insn);
            end if;
            if Enabled (Branch) then
               Res.Include (Branch);
            end if;
            return Res;
         end;
      end if;
   end Coverage_Levels_Enabled;

   ---------------------
   -- Is_Load_Allowed --
   ---------------------

   function Is_Load_Allowed
     (Filename : String; Checkpoint_Levels : Levels_Type) return String is
   begin
      --  Be defensive with object coverage, which is not supported with
      --  checkpoints.

      if (for some L in Object_Coverage_Level => Checkpoint_Levels (L)) then
         return
           ("object coverage in checkpoint is not supported. Corrupted"
            & " checkpoint file?");
      end if;

      --  We allow loading iff the current levels are a subset of
      --  Checkpoint_Levels.

      if (for some L in Source_Coverage_Level =>
            not Checkpoint_Levels (L) and then Levels (L))
      then
         return
           ("incompatible coverage level: "
            & Filename
            & " was produced with """
            & Coverage_Option_Value (Checkpoint_Levels)
            & """ but we expect at least """
            & Coverage_Option_Value
            & """");
      end if;

      return "";
   end Is_Load_Allowed;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context return Context is
      use Ada.Command_Line;

      Command : Unbounded_String := +Support_Files.Gnatcov_Command_Name;
   begin
      for J in 1 .. Argument_Count loop
         Append (Command, ' ' & Argument (J));
      end loop;

      return
        Context'
          (Timestamp => Clock,
           Version   => +Xcov_Version,
           Command   => Command,
           Levels    => +Coverage_Option_Value);
   end Get_Context;

   ---------------
   -- To_String --
   ---------------

   function To_String (C : Context) return String is
      S  : aliased Unbounded_String;
      SS : aliased Unbounded_String_Stream (S'Access);
   begin
      Context'Output (SS'Access, C);
      return +S;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (S : String) return Context is
      Str : aliased Unbounded_String := +S;
      SS  : aliased Unbounded_String_Stream (Str'Access);
   begin
      return Context'Input (SS'Access);
   end From_String;

end Coverage;
