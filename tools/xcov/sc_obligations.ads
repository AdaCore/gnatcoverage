------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

--  Source Coverage Obligations

with Sources; use Sources;
with Strings; use Strings;

package SC_Obligations is

   type SCO_Id is new Natural;
   No_SCO_Id : constant SCO_Id := 0;

   type SCO_Kind is (Statement, Decision, Condition);

   function First_Sloc (SCO : SCO_Id) return Source_Location;
   function Last_Sloc (SCO : SCO_Id) return Source_Location;
   function Kind (SCO : SCO_Id) return SCO_Kind;

   function Image (SCO : SCO_Id) return String;

   function Sloc_To_SCO (Sloc : Source_Location) return SCO_Id;
   --  Return the innermost SCO whose range contains Sloc

   procedure Load_SCOs (ALI_List_Filename : String_Acc);
   --  Load all source coverage obligations for application

end SC_Obligations;
