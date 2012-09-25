------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Command_Line;

package body Switches is

   --------------
   -- Argument --
   --------------

   overriding function Argument
     (S : Command_Line_Switches_Source; Index : Positive) return String
   is
      pragma Unreferenced (S);
   begin
      return Ada.Command_Line.Argument (Index);
   end Argument;

   overriding function Argument
     (S : String_List_Switches_Source; Index : Positive) return String
   is
   begin
      return S.L (S.L'First + Index - 1).all;
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   overriding function Argument_Count
     (S : Command_Line_Switches_Source) return Natural
   is
      pragma Unreferenced (S);
   begin
      return Ada.Command_Line.Argument_Count;
   end Argument_Count;

   overriding function Argument_Count
     (S : String_List_Switches_Source) return Natural
   is
   begin
      return S.L'Length;
   end Argument_Count;

end Switches;
