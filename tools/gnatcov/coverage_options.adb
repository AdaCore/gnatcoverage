------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                  Copyright (C) 2021-2022, AdaCore                        --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Coverage_Options is

   function Level_Options
     (Map : Levels_Option_Maps.Map; Separator : String) return String;
   --  Return a string of all the level options registered in the provided
   --  Map, separated by Separator.

   procedure Add_Source_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of source coverage levels

   procedure Add_Object_Level_Option (L : Levels_Type);
   --  Register L as a valid combination of object coverage levels

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
end Coverage_Options;
