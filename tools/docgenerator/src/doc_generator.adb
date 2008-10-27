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

package body Doc_Generator is
   use Ada.Strings;
   use Ada.Strings.Fixed;

   function Get_Interesting_Substring
     (Str : String;
      Left_Tag, Right_Tag : String) return String
   is
      --  the positions of LEft_Tag and Right_Tag
      Pos1 : Natural := Index
        (Str, Left_Tag) + Left_Tag'Length;
      Pos2 : Natural := Index
        (Str, Right_Tag, Backward) - 1;
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

end Doc_Generator;
