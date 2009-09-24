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
with Traces;  use Traces;

package SC_Obligations is

   type SCO_Id is new Natural;
   No_SCO_Id : constant SCO_Id := 0;
   subtype Valid_SCO_Id is SCO_Id range No_SCO_Id + 1 .. SCO_Id'Last;

   type SCO_Kind is (Statement, Decision, Condition);

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type);
   --  Record Address in SCO's address list

   function Image (SCO : SCO_Id) return String;

   function Slocs_To_SCO
     (First_Sloc, Last_Sloc : Source_Location) return SCO_Id;
   --  Return the innermost SCO whose range overlaps the given range.
   --  It is an error if multiple such SCOs exist and aren't nested.

   procedure Load_SCOs (ALI_List_Filename : String);
   --  Load all source coverage obligations for application

   procedure Report_SCOs_Without_Code;
   --  Output a list of conditions without associated conditional branches

   type Tristate is (False, True, Unknown);
   --  State of a condition, if known

   ----------------------------
   -- Accessors for SCO info --
   ----------------------------

   --  All SCOs

   function Kind       (SCO : SCO_Id) return SCO_Kind;
   function First_Sloc (SCO : SCO_Id) return Source_Location;
   function Last_Sloc  (SCO : SCO_Id) return Source_Location;
   function Parent     (SCO : SCO_Id) return SCO_Id;

   --  Condition SCOs

   function Index (SCO : SCO_Id) return Natural;

   --  Decision SCOs

   function Last_Cond_Index (SCO : SCO_Id) return Natural;

end SC_Obligations;
