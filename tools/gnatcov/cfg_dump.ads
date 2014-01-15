------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

with Ada.Containers.Indefinite_Vectors;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Symbols;

with Inputs;
with Slocs;  use Slocs;
with Traces; use Traces;

package CFG_Dump is

   --  Locations are the way for the user to specify what to include in a CFG.
   --  There are two classes of locations: the first one (User_Location) is how
   --  the user describes what he wants, and the other one (Proc_Location) is
   --  a translation that is actually used to efficiently process its request.
   --  There are several kind of locations so that the user can accurately
   --  select what he wants to include.

   type User_Location_Kind is
     (Sloc_Range,
      Around_Address,
      Address_Range,
      Symbol);

   type User_Location (Kind : User_Location_Kind) is record
      case Kind is
         when Sloc_Range =>
            Sloc_Range : Source_Location_Range;
         when Around_Address =>
            Address : Pc_Type;
         when Address_Range =>
            PC_First, PC_Last : Pc_Type;
         when Symbol =>
            Name : GNATCOLL.Symbols.Symbol;
      end case;
   end record;
   --  Information the user provides to match code to include into a CFG

   type Proc_Location_Kind is
     (Address_Range,
      Sloc_Range);

   type Proc_Location (Kind : Proc_Location_Kind) is record
      case Kind is
         when Address_Range =>
            PC_First, PC_Last : Pc_Type;
         when Sloc_Range =>
            Sloc_Range : Source_Location_Range;
      end case;
   end record;
   --  Internal information used to match code to include into a CFG

   No_Proc_Location : constant Proc_Location :=
     (Kind     => Address_Range,
      PC_First => 1,
      PC_Last  => 0);

   function Parse_User_Location (S : String) return User_Location;
   --  Decode an user location from its textual form. Three formats are
   --  accepted:
   --
   --  If the location contains a colon (':'), it must be a sloc range in the
   --  form: "file:line1:col1-line2:col2"
   --
   --  Else, if the location starts with '@', the rest of the string must be
   --  an hexadecimal address (PC). It then targets the address range for the
   --  symbol this address belongs to.
   --
   --  Else, if the location contains two consecutive dots (".."), it must be
   --  an address range in the form: "address1..address2", both addresses being
   --  hexadecimal numbers.
   --
   --  Otherwise, the location is accepted as a symbol name.

   type Output_Format is (None, Dot, SVG, PDF, PNG);
   --  Format used to output the CFG. None stands for "output the Dot graph
   --  without passing it to dot(1)".

   package User_Location_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => User_Location);

   subtype User_Locations is User_Location_Vectors.Vector;

   procedure Dump (Exec_Path         : String;
                   Locations         : User_Locations;
                   Output            : String_Access;
                   Format            : Output_Format;
                   SCO_Files_List    : Inputs.Inputs_Type;
                   Traces_Files_List : Inputs.Inputs_Type;
                   Keep_Edges        : Boolean);

end CFG_Dump;
