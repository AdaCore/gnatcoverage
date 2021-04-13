------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Ordered_Maps;

with GNAT.Strings; use GNAT.Strings;

with Strings;       use Strings;
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

   package Levels_Option_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => GNAT.Strings.String_Access,
        Element_Type => Levels_Type,
        "<"          => Strings."<");
   Source_Levels_Option_Map : Levels_Option_Maps.Map;
   Object_Levels_Option_Map : Levels_Option_Maps.Map;

   Object_Coverage_Enabled_Cached : Boolean;
   Source_Coverage_Enabled_Cached : Boolean;
   MCDC_Coverage_Enabled_Cached   : Boolean;
   --  Global variables to hold cached return values for enabled coverage
   --  levels query functions. These functions can be called very often, so
   --  just returning a boolean removes any overhead. These globals are updated
   --  at each call of Set_Coverage_Levels, which is not called very often.

   procedure Add_Source_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of source coverage levels

   procedure Add_Object_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of object coverage levels

   function Level_Options
     (Map : Levels_Option_Maps.Map; Separator : String) return String;
   --  Return a string of all the level options registered in the provided
   --  Map, separated by Separator.

   function Any_Coverage_Enabled (L : Levels_Type) return Boolean;
   --  True if any level marked True in L is enabled

   -----------------------------
   -- Add_Source_Level_Option --
   -----------------------------

   procedure Add_Source_Level_Option (L : Levels_Type) is
   begin
      Source_Levels_Option_Map.Insert
        (new String'(Coverage_Option_Value (L)), L);
   end Add_Source_Level_Option;

   -----------------------------
   -- Add_Object_Level_Option --
   -----------------------------

   procedure Add_Object_Level_Option (L : Levels_Type) is
   begin
      Object_Levels_Option_Map.Insert
        (new String'(Coverage_Option_Value (L)), L);
   end Add_Object_Level_Option;

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

      Object_Coverage_Enabled_Cached := Any_Coverage_Enabled
        ((Object_Coverage_Level => True, others => False));
      Source_Coverage_Enabled_Cached := Any_Coverage_Enabled
        ((Source_Coverage_Level => True, others => False));
      MCDC_Coverage_Enabled_Cached := Any_Coverage_Enabled
        ((MCDC_Coverage_Level => True, others => False));
   end Set_Coverage_Levels;

   --------------------
   -- Current_Levels --
   --------------------

   function Current_Levels return Levels_Type is
   begin
      return Levels;
   end Current_Levels;

   ---------------------
   -- Is_Load_Allowed --
   ---------------------

   function Is_Load_Allowed
     (Filename : String; Checkpoint_Levels : Levels_Type) return String is
   begin
      --  Be defensive with object coverage, which is not supported with
      --  checkpoints.

      if (for some L in Object_Coverage_Level => Checkpoint_Levels (L)) then
         return ("object coverage in checkpoint is not supported. Corrupted"
                 & " checkpoint file?");
      end if;

      --  We allow loading iff the current levels are a subset of
      --  Checkpoint_Levels.

      if (for some L in Source_Coverage_Level =>
             not Checkpoint_Levels (L) and then Levels (L))
      then
         return ("incompatible coverage level: " & Filename
                 & " was produced with """
                 & Coverage.Coverage_Option_Value (Checkpoint_Levels)
                 & """ but we expect at least """
                 & Coverage.Coverage_Option_Value & """");
      end if;

      return "";
   end Is_Load_Allowed;

   ---------------------------
   -- Coverage_Option_Value --
   ---------------------------

   function Coverage_Option_Value (L : Levels_Type) return String is
      Option : Unbounded_String;
   begin
      for J in L'Range loop
         if L (J) then
            if Length (Option) /= 0 then
               Append (Option, '+');
            end if;
            Append (Option, To_Lower (J'Img));
         end if;
      end loop;
      return To_String (Option);
   end Coverage_Option_Value;

   -------------------
   -- Level_Options --
   -------------------

   function Level_Options
     (Map : Levels_Option_Maps.Map; Separator : String) return String
   is
      Options : Unbounded_String;

      use Levels_Option_Maps;

      procedure Put_Option (Cur : Cursor);
      --  Add description of option to Options

      ----------------
      -- Put_Option --
      ----------------

      procedure Put_Option (Cur : Cursor) is
      begin
         if Length (Options) /= 0 then
            Append (Options, Separator);
         end if;
         Append (Options, Key (Cur).all);
      end Put_Option;

   --  Start of processing for Level_Options

   begin
      Map.Iterate (Put_Option'Access);
      return To_String (Options);
   end Level_Options;

   --------------------------
   -- Object_Level_Options --
   --------------------------

   function Object_Level_Options (Separator : String) return String is
   begin
      return Level_Options (Object_Levels_Option_Map, Separator);
   end Object_Level_Options;

   --------------------------
   -- Source_Level_Options --
   --------------------------

   function Source_Level_Options (Separator : String) return String is
   begin
      return Level_Options (Source_Levels_Option_Map, Separator);
   end Source_Level_Options;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context return Context is
      use Ada.Command_Line;

      Command : Unbounded_String :=
         To_Unbounded_String (Support_Files.Gnatcov_Command_Name);
   begin
      for J in 1 .. Argument_Count loop
         Append (Command, ' ' & Argument (J));
      end loop;

      return Context'
        (Timestamp => Clock,
         Version   => To_Unbounded_String (Xcov_Version),
         Command   => Command,
         Levels    => To_Unbounded_String (Coverage_Option_Value));
   end Get_Context;

   ---------------
   -- To_String --
   ---------------

   function To_String (C : Context) return String is
      US  : aliased Unbounded_String;
      USS : aliased Unbounded_String_Stream (US'Access);
   begin
      Context'Output (USS'Access, C);
      return To_String (US);
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (S : String) return Context is
      US  : aliased Unbounded_String := To_Unbounded_String (S);
      USS : aliased Unbounded_String_Stream (US'Access);
   begin
      return Context'Input (USS'Access);
   end From_String;

begin
   --  Register command line options for valid combinations of coverage levels

   --  Object coverage levels

   Add_Object_Level_Option ((Insn   => True,
                             others => False));
   Add_Object_Level_Option ((Branch => True,
                             others => False));

   --  Source coverage levels

   Add_Source_Level_Option ((Stmt     => True,
                             others   => False));
   Add_Source_Level_Option ((Stmt     => True,
                             Decision => True,
                             others   => False));
   Add_Source_Level_Option ((Stmt     => True,
                             MCDC     => True,
                             others   => False));
   Add_Source_Level_Option ((Stmt     => True,
                             UC_MCDC  => True,
                             others   => False));
end Coverage;
