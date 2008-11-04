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

with Ada.Strings.Unbounded;

package Doc_Generator is

   --  enumeration specifying possible coverage values for a subprogram
   type Function_Coverage is
     (COVERED, NOT_COVERED, PARTIALLY_COVERED);

   --  enumeration specifying possible coverage values for a source line
   type Line_Coverage is
     (COVERED, NOT_CONVERED, PARTIALLY_COVERED);

   Coverage_Values : array (Function_Coverage'Range) of String (1 .. 1) :=
     (COVERED => "+",
      NOT_COVERED => "-",
      PARTIALLY_COVERED => "!");

   --  return the expected coverage value contained in the string
   --  looks for the values: "!", "+", "-"
   function Get_Coverage (S : String) return Function_Coverage;

   --  Return the substring of Str between Left_tag and Rigth_Tag;
   --  if Left_Tag or Right_Tag are not found, it returns an empty string
   function Get_Interesting_Substring
     (Str : String;
      Left_Tag, Right_Tag : String) return String;

   --  Return the position of Tag within Str
   procedure Starts_With
     (Str : String; Tag : String; Pos : out Natural);

   --  Removes Prefix from S
   function Remove_Prefix
     (S : Ada.Strings.Unbounded.Unbounded_String;
      Prefix : String) return Ada.Strings.Unbounded.Unbounded_String;

   --  Remove the standard prefix (see private part) from S
   function Remove_Std_prefix
     (S : in Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String;

   --  Remove To_Remove from S (if present)
   function Remove (S : String; To_Remove : String) return String;

   function Replace_All (S : String; Pattern : String; By : String)
     return String;

   procedure To_Lower (S : in out String);

private

   Std_Prefix : constant String := "--@";

   --  Show_Detailed_Info : String := "none";
   Show_Detailed_Info : String := "block";

end Doc_Generator;
