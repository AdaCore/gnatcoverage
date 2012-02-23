------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

with Strings; use Strings;

package body Coverage is

   type Levels_Type is array (Coverage_Level) of Boolean;

   function To_Option (L : Levels_Type) return String;
   --  Option string for the combination of levels L

   package Coverage_Option_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => GNAT.Strings.String_Access,
        Element_Type => Levels_Type,
        "<"          => Strings."<");
   Coverage_Option_Map : Coverage_Option_Maps.Map;

   Levels : Levels_Type := (others => False);
   Levels_Set : Boolean := False;
   --  Global variable that records the coverage operation that has been asked
   --  to xcov. This should be modified only one time by Set_Coverage_Levels.

   procedure Add_Coverage_Option (L : Levels_Type);
   --  Register L as a valid combination of coverage levels

   function Any_Coverage_Enabled (L : Levels_Type) return Boolean;
   --  True if any level marked True in L is enabled

   -------------------------
   -- Add_Coverage_Option --
   -------------------------

   procedure Add_Coverage_Option (L : Levels_Type) is
   begin
      Coverage_Option_Map.Insert (new String'(To_Option (L)), L);
   end Add_Coverage_Option;

   ---------------------------
   -- Coverage_Option_Value --
   ---------------------------

   function Coverage_Option_Value return String is
   begin
      return To_Option (Levels);
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

   ---------------------------
   -- MCDC_Coverage_Enabled --
   ---------------------------

   function MCDC_Coverage_Enabled return Boolean is
   begin
      return Any_Coverage_Enabled
        ((MCDC_Coverage_Level => True, others => False));
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
      return Any_Coverage_Enabled
        ((Object_Coverage_Level => True, others => False));
   end Object_Coverage_Enabled;

   -----------------------------
   -- Source_Coverage_Enabled --
   -----------------------------

   function Source_Coverage_Enabled return Boolean is
   begin
      return Any_Coverage_Enabled
        ((Source_Coverage_Level => True, others => False));
   end Source_Coverage_Enabled;

   -------------------------
   -- Set_Coverage_Levels --
   -------------------------

   procedure Set_Coverage_Levels (Opt : String) is
   begin
      pragma Assert (not Levels_Set);
      Levels := Coverage_Option_Map.Element (Opt'Unrestricted_Access);
      Levels_Set := True;
   end Set_Coverage_Levels;

   ---------------
   -- To_Option --
   ---------------

   function To_Option (L : Levels_Type) return String is
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
   end To_Option;

   ----------------------------
   -- Valid_Coverage_Options --
   ----------------------------

   function Valid_Coverage_Options return String is
      Options : Unbounded_String;

      use Coverage_Option_Maps;

      procedure Put_Option (Cur : Cursor);
      --  Add description of option to Options

      ----------------
      -- Put_Option --
      ----------------

      procedure Put_Option (Cur : Cursor) is
      begin
         if Length (Options) /= 0 then
            Append (Options, '|');
         end if;
         Append (Options, Key (Cur).all);
      end Put_Option;

   --  Start of processing for Valid_Coverage_Options

   begin
      Coverage_Option_Map.Iterate (Put_Option'Access);
      return To_String (Options);
   end Valid_Coverage_Options;

begin
   --  Register command line options for valid combinations of coverage levels

   Add_Coverage_Option ((Insn   => True,                   others => False));
   Add_Coverage_Option ((Branch => True,                   others => False));
   Add_Coverage_Option ((Stmt   => True,                   others => False));
   Add_Coverage_Option ((Stmt   => True, Decision => True, others => False));
   Add_Coverage_Option ((Stmt   => True, MCDC     => True, others => False));
   Add_Coverage_Option ((Stmt   => True, UC_MCDC  => True, others => False));
end Coverage;
