------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;

with GNAT.Strings;

limited with SC_Obligations;
with Strings; use Strings;

package Coverage_Options is

   use all type Unbounded_String;

   type Coverage_Level is
     (Insn, Branch, Stmt, Decision, MCDC, UC_MCDC, ATC, ATCC, Fun_Call, GExpr);
   --  Coverage objectives supported by xcov. The following values are
   --  supported:

   --  * object coverage at instruction level         (Insn);
   --  * object coverage at branch level              (Branch);
   --  * source coverage at statement level           (Stmt);
   --  * source coverage at decision level            (Decision);
   --  * source coverage at masking MC/DC level       (MCDC);
   --  * source coverage at unique cause MC/DC level  (UC_MCDC);
   --  * source coverage at ATC level                 (ATC);
   --  * source coverage at ATCC level                (ATCC);
   --  * source coverage at Call level                (Fun_Call);
   --  * source coverage at Guarded Expressions level (GExpr).

   --  The terms "instruction", "branch", "statement", "decision" and "MCDC"
   --  should be understood here as they are defined in the DO-178B standard;
   --  their meaning is also documented in Couverture's documentation.
   --
   --  The meaning of the assertion coverage level "ATC" should be understood
   --  as follows:
   --
   --  * "ATC"  : the whole expression has been evaluated to True at least
   --             once;
   --  * "ATCC" : all the conditions of the decision have been evaluated at
   --             least once as part of an evaluation to True of the whole
   --             decision.
   --
   --  The "Fun_Call" coverage criteria mesures the coverage of calls and that
   --  of functions. They must be understood as:
   --
   --  * Function coverage : a function (for Ada, understand "subprogram") is
   --                        covered if it was entered at least once;
   --
   --  * Call coverage     : a call is covered if it was executed as least
   --                        once.
   --
   --  The "GExpr" coverage criteria (short for Guarded Expressions) serves the
   --  purpose of tracking the exhaustive execution of all branches of
   --  conditional expressions (case-expr and if-expr) and to ensure the
   --  execution the dependent expression of quantified expressions
   --  (for some and for all), which may not run if executed against an empty
   --  array.

   subtype Object_Coverage_Level is Coverage_Level range Insn .. Branch;
   subtype Source_Coverage_Level is Coverage_Level range Stmt .. GExpr;
   subtype MCDC_Coverage_Level is Coverage_Level range MCDC .. UC_MCDC;
   subtype Contract_Condition_Level is Coverage_Level range ATCC .. ATCC;

   type Levels_Type is array (Coverage_Level) of Boolean;
   --  Set of Coverage_Levels

   package Levels_Option_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => GNAT.Strings.String_Access,
        Element_Type => Levels_Type,
        "<"          => Strings."<");
   Source_Levels_Option_Map : Levels_Option_Maps.Map;
   Object_Levels_Option_Map : Levels_Option_Maps.Map;

   function Coverage_Option_Value (L : Levels_Type) return String;
   --  Return the coverage option value corresponding to L

   function Source_Level_Options return String;
   --  Return a regular expression string describing the valid --level
   --  combinations of source coverage levels.

   function Object_Level_Options (Separator : String) return String;
   --  Return a string expression describing valid --level alternatives
   --  for object coverage, separated by Separator.

   function Annotation_Kind_Options return String;
   --  Return the list of accepted values for the --kind switch for the gnatcov
   --  annotate command.

   function Annot_Kind_Relevant_Switches return String;
   --  Return the switches relevant for each kind, in the form
   --
   --  <Annotation_Kind>: Required_Switches .. [Optional_Switches]
   --
   --  One annotation kind on its own line.

   function Kind_Image
     (Kind : SC_Obligations.Any_Annotation_Kind) return String;
   --  Return the corresponding enumeration literal string, in usual Ada case.
   --
   --  Located here due to elaboration constraints.

end Coverage_Options;
