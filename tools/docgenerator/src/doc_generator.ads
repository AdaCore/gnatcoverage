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

   --  Return the substring of Str between Left_tag and Rigth_Tag;
   --  if Left_Tag or Right_Tag are not found, it returns an empty string
   function Get_Interesting_Substring
     (Str : String;
      Left_Tag, Right_Tag : String) return String;

   --  Return the position of Tag within Str
   procedure Starts_With
     (Str : String; Tag : String; Pos : out Natural);
   pragma Inline (Starts_With);

   --  Removes Prefix from S
   function Remove_Prefix
     (S : Ada.Strings.Unbounded.Unbounded_String;
      Prefix : String) return Ada.Strings.Unbounded.Unbounded_String;

   --  Remove the standard prefix (see private part) from S
   function Remove_Std_prefix
     (S : in Ada.Strings.Unbounded.Unbounded_String)
     return Ada.Strings.Unbounded.Unbounded_String;

private

   Std_Prefix : constant String := "--@";

end Doc_Generator;
