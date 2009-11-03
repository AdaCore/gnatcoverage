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

with Slocs;   use Slocs;
with Traces;  use Traces;

package SC_Obligations is

   type SCO_Id is new Natural;
   No_SCO_Id : constant SCO_Id := 0;
   subtype Valid_SCO_Id is SCO_Id range No_SCO_Id + 1 .. SCO_Id'Last;

   type SCO_Kind is (Statement, Decision, Condition);

   procedure Add_Address (SCO : SCO_Id; Address : Pc_Type);
   --  Record Address in SCO's address list

   function Image (SCO : SCO_Id) return String;

   function Sloc_To_SCO (Sloc : Source_Location) return SCO_Id;
   --  Return the innermost SCO whose range contains the given sloc.
   --  It is an error if multiple such SCOs exist and aren't nested.
   --  Note: if Sloc has a null column number, returns an unspecified SCO
   --  among those covering that line.
   --  For No_Location, return No_SCO_Id.

   function Has_SCO
     (Sloc_Begin : Source_Location;
      Sloc_End   : Source_Location) return Boolean;
   --  Return True if there is at least one SCO whose range has a non-null
   --  intersection with Sloc_Begin .. Sloc_End.

   procedure Load_SCOs (ALI_Filename : String);
   --  Load source coverage obligations from ALI_Filename

   procedure Report_SCOs_Without_Code;
   --  Output a list of conditions without associated conditional branches

   type Tristate is (False, True, Unknown);
   subtype Known_Tristate is Tristate range False .. True;
   --  State of a condition, if known

   To_Tristate : constant array (Boolean) of Known_Tristate :=
                   (False => False, True => True);

   To_Boolean : constant array (Known_Tristate) of Boolean :=
                   (False => False, True => True);

   type Any_Condition_Index is new Integer range -1 .. Integer'Last;
   No_Condition_Index : constant Any_Condition_Index := -1;
   subtype Condition_Index is
     Any_Condition_Index range 0 .. Any_Condition_Index'Last;

   ----------------------------
   -- Accessors for SCO info --
   ----------------------------

   --  All SCOs

   function Kind       (SCO : SCO_Id) return SCO_Kind;
   function First_Sloc (SCO : SCO_Id) return Source_Location;
   function Last_Sloc  (SCO : SCO_Id) return Source_Location;
   function Parent     (SCO : SCO_Id) return SCO_Id;

   --  Statement SCOs

   function Previous   (SCO : SCO_Id) return SCO_Id;

   --  Condition SCOs

   function Index (SCO : SCO_Id) return Condition_Index;

   function Next_Condition (SCO : SCO_Id; Value : Boolean) return SCO_Id;
   --  Next condition to be tested, depending of value of this condition,
   --  or No_SCO_Id if the value determines the decision outcome.

   function Outcome (SCO : SCO_Id; Value : Boolean) return Tristate;
   --  Outcome of decision if this condition has the given value, or Unknown
   --  if the value does not determine the decision outcome.

   --  Decision SCOs

   function Condition (SCO : SCO_Id; Index : Condition_Index) return SCO_Id;
   function Last_Cond_Index (SCO : SCO_Id) return Condition_Index;

end SC_Obligations;
