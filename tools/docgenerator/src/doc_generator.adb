------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

package body Doc_Generator is
   use Ada.Strings;
   use Ada.Strings.Fixed;

   function Get_Coverage (S : String) return Function_Coverage
   is
      Pos : Natural := 0;
      --  look for the ":" character and start looking for coverage value
      --  from there
      Start_Pos : Natural := Index (S, ":");
      Val : Function_Coverage := NOT_COVERED;
   begin

      --  loop over all possible character representing a coverage value
      for V in Function_Coverage'Range loop
         --  exit in advance if a result has been found
         exit when Pos > 0;
         Pos := Index (S, Coverage_Values (V), Start_Pos);
         Val := V;
      end loop;

      if Pos = 0 then
         --  if no result has been found, then the spec were badly written
         raise Program_Error;
      else
         return Val;
      end if;
   end Get_Coverage;


   function Get_Interesting_Substring
     (Str : String;
      Left_Tag, Right_Tag : String) return String
   is
      --  the positions of Left_Tag and Right_Tag
      Pos1 : Natural := Index
        (Str, Left_Tag) + Left_Tag'Length;
      Pos2 : Natural := Index
        (Str, Right_Tag, Pos1) - 1;
   begin
      --  return the substring
      return Ada.Strings.Unbounded.Slice
        (Ada.Strings.Unbounded.To_Unbounded_String (Str), Pos1, Pos2);
   end Get_Interesting_Substring;

   function Remove_Prefix
     (S : Ada.Strings.Unbounded.Unbounded_String;
      Prefix : String) return Ada.Strings.Unbounded.Unbounded_String is
      --  The position of the Prefix
      Pos : Positive := Ada.Strings.Unbounded.Index
        (S, Prefix);
      use Ada.Strings.Unbounded;
   begin
      --  If the prefix is present, remove it
      if Pos > 0 then
         return Ada.Strings.Unbounded.To_Unbounded_String
           (Ada.Strings.Unbounded.Slice (S, Prefix'Length + 1, Length (S)));
      else
         return S;
      end if;
   end Remove_Prefix;

   function Remove_Std_prefix
     (S : in Ada.Strings.Unbounded.Unbounded_String) return
   Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Remove_Prefix
        (Ada.Strings.Unbounded.Trim (S, Ada.Strings.Both), Std_Prefix);
   end Remove_Std_prefix;

   procedure Starts_With
     (Str : String; Tag : String; Pos : out Natural) is
   begin
      Pos := Index (Str, Tag);
   end Starts_With;

   function Remove (S : String; To_Remove : String) return String is
      Pos : Natural := Index (S, To_Remove);
   begin
      if Pos > 0 then
         return Delete (S, Pos, Pos + To_Remove'Length);
      else
         return S;
      end if;
   end Remove;

   function Replace_All (S : String; Pattern : String; By : String)
                         return String is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      Res : Ada.Strings.Unbounded.Unbounded_String
        := To_Unbounded_String (S);
      Pos : Natural := 0;
   begin
      loop
         Pos := Index (Res, Pattern);
         exit when Pos = 0;
         --  Ada.Strings.Unbounded.Text_IO.Put_Line (Res);
         Res := Ada.Strings.Unbounded.Replace_Slice
           (Res, Pos, Pos + Pattern'Length - 1, By);
      end loop;
      return To_String (Res);
   end Replace_All;

   procedure To_Lower (S : in out String) is
   begin
      for I in S'Range loop
         S (I) := Ada.Strings.Maps.Value
           (Ada.Strings.Maps.Constants.Lower_Case_Map, S (I));
      end loop;
   end To_Lower;


end Doc_Generator;
