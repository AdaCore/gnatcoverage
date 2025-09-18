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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with SC_Obligations;

package body Coverage_Options is

   function Level_Options
     (Map : Levels_Option_Maps.Map; Separator : String) return String;
   --  Return a string of all the level options registered in the provided
   --  Map, separated by Separator.

   procedure Add_Source_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of source coverage levels

   procedure Add_Object_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of object coverage levels

   procedure Add_Source_Coverage_Level_Combinaisons;
   --  Register the possible source coverage level combinaisons

   ---------------------------
   -- Coverage_Option_Value --
   ---------------------------

   function Coverage_Option_Value (L : Levels_Type) return String is
      Option : Unbounded_String;
   begin
      for J in L'Range loop
         if L (J) then
            if Option /= "" then
               Append (Option, '+');
            end if;
            Append (Option, To_Lower (J'Img));
         end if;
      end loop;
      return +Option;
   end Coverage_Option_Value;

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
         if Options /= "" then
            Append (Options, Separator);
         end if;
         Append (Options, Key (Cur).all);
      end Put_Option;

      --  Start of processing for Level_Options

   begin
      Map.Iterate (Put_Option'Access);
      return +Options;
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

   function Source_Level_Options return String is
      function Level_Str (L : Source_Coverage_Level) return String;
      --  Return the lowercase string representation of L

      function Level_Str (L : Source_Coverage_Level) return String
      is (To_Lower (L'Img));
   begin
      return
        Level_Str (Stmt)
        & "(+"
        & Level_Str (Decision)
        & "|"
        & "+"
        & Level_Str (MCDC)
        & "|"
        & "+"
        & Level_Str (UC_MCDC)
        & ")?"
        & "(+"
        & Level_Str (ATC)
        & "|"
        & "+"
        & Level_Str (ATCC)
        & ")?"
        & "(+"
        & Level_Str (Fun_Call)
        & ")?"
        & "(+"
        & Level_Str (GExpr)
        & ")?";
   end Source_Level_Options;

   -------------------------------------
   -- Add_Coverage_Level_Combinaisons --
   -------------------------------------

   procedure Add_Source_Coverage_Level_Combinaisons is
      procedure Add_Assert_Coverage_Levels (Combinaison : Levels_Type);
      --  Register the coverage level Combinaison combined with each level
      --  of assertion coverage.

      procedure Add_Fun_Call_Coverage_Level (Combinaison : Levels_Type);
      --  Register the coverage level Combinaison combined with function and
      --  call coverage.

      procedure Add_GExpr_Coverage_Level (Combinaison : Levels_Type);
      --  Register the coverage level Combinaison combined with guarded
      --  expression coverage.

      ---------------------------------
      -- Add_Fun_Call_Coverage_Level --
      ---------------------------------

      procedure Add_Fun_Call_Coverage_Level (Combinaison : Levels_Type) is
         Comb : Levels_Type := Combinaison;
      begin
         Comb (Fun_Call) := True;
         Add_Source_Level_Option (Comb);
      end Add_Fun_Call_Coverage_Level;

      --------------------------------
      -- Add_Assert_Coverage_Levels --
      --------------------------------

      procedure Add_Assert_Coverage_Levels (Combinaison : Levels_Type) is
         Assert_Levels : constant array (1 .. 2) of Coverage_Level :=
           (ATC, ATCC);
         Comb          : Levels_Type := Combinaison;
      begin
         for Lvl of Assert_Levels loop

            --  Activate Lvl to combine it with the levels in Comb
            Comb (Lvl) := True;

            Add_Source_Level_Option (Comb);

            --  Add CallFunc to the current combinaison
            Add_Fun_Call_Coverage_Level (Comb);

            --  Deactivate Lvl to add the next assertion level to the
            --  combinaison Comb
            Comb (Lvl) := False;

         end loop;
      end Add_Assert_Coverage_Levels;

      ------------------------------
      -- Add_GExpr_Coverage_Level --
      ------------------------------

      procedure Add_GExpr_Coverage_Level (Combinaison : Levels_Type) is
         Comb : Levels_Type := Combinaison;
      begin
         --  Activate GExpr to combine it with the levels in Comb
         Comb (GExpr) := True;

         Add_Source_Level_Option (Comb);
         Add_Fun_Call_Coverage_Level (Comb);
         Add_Assert_Coverage_Levels (Comb);
      end Add_GExpr_Coverage_Level;

      Decision_Levels : constant array (1 .. 3) of Coverage_Level :=
        (Decision, MCDC, UC_MCDC);

      Combinaison : Levels_Type := (Stmt => True, others => False);
   begin

      --  Add "stmt" only. Combine it alone with "callfunc", and then with all
      --  possible assertion levels.

      Add_Source_Level_Option (Combinaison);
      Add_Fun_Call_Coverage_Level (Combinaison);
      Add_Assert_Coverage_Levels (Combinaison);
      Add_GExpr_Coverage_Level (Combinaison);

      --  Do the same for all other regular source coverage options

      for Lvl of Decision_Levels loop

         --  Activate Lvl to combine it with "stmt"
         Combinaison (Lvl) := True;

         Add_Source_Level_Option (Combinaison);
         Add_Fun_Call_Coverage_Level (Combinaison);
         Add_Assert_Coverage_Levels (Combinaison);
         Add_GExpr_Coverage_Level (Combinaison);

         --  Deactivate Lvl to combine the next level with "stmt"
         Combinaison (Lvl) := False;

      end loop;
   end Add_Source_Coverage_Level_Combinaisons;

   function Annotation_Kind_Options return String is
      use SC_Obligations;
      Res : Unbounded_String := +Kind_Image (Exempt_Region);
   begin
      for Kind in Exempt_On .. Any_Annotation_Kind'Last loop
         Res := Res & ", " & Kind_Image (Kind);
      end loop;
      return +Res;
   end Annotation_Kind_Options;

   ----------------------------------
   -- Annot_Kind_Relevant_Switches --
   ----------------------------------

   function Annot_Kind_Relevant_Switches return String is
      use SC_Obligations;
      Res : Unbounded_String;
   begin
      for Kind in Exempt_Region .. Any_Annotation_Kind'Last loop
         Res :=
           Res & (+(ASCII.LF & ASCII.LF & "   " & Kind_Image (Kind) & ": "));
         case Kind is
            when Exempt_On     =>
               Res := Res & "--location=LINE:COL --justification=MESSAGE";

            when Exempt_Off    =>
               Res := Res & "--location=LINE:COL";

            when Exempt_Region =>
               Res :=
                 Res
                 & "--start-location=LINE:COL --end-location=LINE:COL"
                 & " --justification=MESSAGE";

            when Dump_Buffers  =>
               Res :=
                 Res
                 & "--location=LINE:COL [--annotate-after]"
                 & " [--dump-filename-prefix=TEXT]";

            when Reset_Buffers =>
               Res := Res & "--location=LINE:COL [--annotate-after]";

            when Cov_On        =>
               Res := Res & "--location=LINE:COL";

            when Cov_Off       =>
               Res := Res & "--location=LINE:COL [--justification=MESSAGE]";
         end case;
      end loop;
      return US.To_String (Res);
   end Annot_Kind_Relevant_Switches;

   ----------------
   -- Kind_Image --
   ----------------

   function Kind_Image
     (Kind : SC_Obligations.Any_Annotation_Kind) return String
   is (case Kind is
         when SC_Obligations.Unknown       => "Unknown",
         when SC_Obligations.Exempt_Region => "Exempt_Region",
         when SC_Obligations.Exempt_On     => "Exempt_On",
         when SC_Obligations.Exempt_Off    => "Exempt_Off",
         when SC_Obligations.Dump_Buffers  => "Dump_Buffers",
         when SC_Obligations.Reset_Buffers => "Reset_Buffers",
         when SC_Obligations.Cov_Off       => "Cov_Off",
         when SC_Obligations.Cov_On        => "Cov_On");

begin
   --  Register command line options for valid combinations of coverage levels

   --  Object coverage levels

   Add_Object_Level_Option ((Insn => True, others => False));
   Add_Object_Level_Option ((Branch => True, others => False));

   --  Source coverage levels

   Add_Source_Coverage_Level_Combinaisons;
end Coverage_Options;
