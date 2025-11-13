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

--  This package provides primitives to handle coverage Levels that can be
--  performed by xcov: how to parse them from the command line and how to print
--  them into reports.
--  This package also keeps the track of the coverage Level selected
--  for one run; this Level will be unique and will not change after
--  it has been initialized.

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Ordered_Sets;

with Coverage_Options; use Coverage_Options;
with Strings;          use Strings;

package Coverage is

   use all type Unbounded_String;

   function Image (Level : Coverage_Level) return String
   is (case Level is
         when Insn     => "Insn",
         when Branch   => "Branch",
         when Stmt     => "Stmt",
         when Decision => "Decision",
         when MCDC     => "MCDC",
         when UC_MCDC  => "UC_MCDC",
         when ATC      => "ATC",
         when ATCC     => "ATCC",
         when Fun_Call => "Function and call",
         when GExpr    => "Guarded expression");
   --  Case sensitive version of Coverage_Level'Image

   procedure Set_Coverage_Levels (Opt : String);
   --  Set the coverage levels to be assessed by xcov

   function Enabled (Level : Coverage_Level) return Boolean;
   --  True when Level is enabled

   function Object_Coverage_Enabled return Boolean;
   --  True when any Object_Coverage_Level is enabled

   function Source_Coverage_Enabled return Boolean;
   --  True if any Source_Coverage_Level is enabled

   function Decision_Coverage_Enabled return Boolean;
   --  True if decision coverage is enabled (either via decision, or MC/DC)

   function MCDC_Coverage_Enabled return Boolean;
   --  True if any MCDC_Coverage_Level is enabled

   function Assertion_Condition_Coverage_Enabled return Boolean;
   --  True if any level of assertion coverage requiring to compute the
   --  coverage of separate conditions is enabled;

   function Assertion_Coverage_Enabled return Boolean;
   --  True if any Assertion_Coverage_Level is enabled

   function Object_Level return Object_Coverage_Level;
   --  If Object_Coverage_Enabled, return Insn or Branch, depending on which
   --  one is enabled (it is illegal to have both enabled).

   function MCDC_Level return MCDC_Coverage_Level;
   --  If MCDC_Coverage_Enabled, return MCDC or UC_MCDC, depending on
   --  which one is enabled (it is illegal to have both enabled).

   function Assertion_Condition_Level return Contract_Condition_Level;
   --  If Assertion_Condition_Coverage_Enabled, return ATCC.

   function Coverage_Option_Value return String;
   --  Return the coverage option value for the currently enabled levels

   function Current_Levels return Levels_Type;
   --  Return the set of coverage levels currently enabled. Note that this
   --  function is meant to be used only for the purpose of generating a
   --  checkpoint. Code that needs to test the enabled levels should instead
   --  use the above *_Enabled functions.

   package Levels_Sets is new
     Ada.Containers.Ordered_Sets (Element_Type => Coverage_Level);

   function Source_Levels_Enabled return Levels_Sets.Set;
   --  Return the set of source coverage levels enabled. Differs from the above
   --  as it will return the coverage levels implicitely enabled (such as
   --  decision when passing stmt+mcdc).

   function Coverage_Levels_Enabled return Levels_Sets.Set;
   --  Return the set of coverage levels enabled. Similar as the above but
   --  including object coverage levels.

   function Is_Load_Allowed
     (Filename : String; Checkpoint_Levels : Levels_Type) return String;
   --  If loading a checkpoint state (from Filename) with the given coverage
   --  levels for the current enabled levels is allowed, return an empty
   --  string. Otherwise, return an error message.

   ----------------------
   -- Coverage context --
   ----------------------

   --  This type captures all information related to one execution of
   --  GNATCOV COVERAGE.

   type Context is record
      Timestamp : Time;
      --  Timestamp of report generation

      Version : Unbounded_String;
      --  Gnatcov version

      Command : Unbounded_String;
      --  Gnatcov command line

      Levels : Unbounded_String;
      --  Coverage option for enabled coverage levels
   end record;
   type Context_Access is access all Context;

   function Get_Context return Context;
   --  Return the description of the current coverage assessment context.
   --  Note: this must be called after the files table has been filled.

   function To_String (C : Context) return String;
   function From_String (S : String) return Context;
   --  Convert between contexts and string for storage purposes

end Coverage;
