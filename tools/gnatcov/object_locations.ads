------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with GNATCOLL.Symbols;

with Slocs;      use Slocs;
with Traces;     use Traces;
with Traces_Elf; use Traces_Elf;

package Object_Locations is

   --  Locations are the way for the user to select instructions in object
   --  code. There are two classes of locations: the first one (User_Location)
   --  is how the user describes what he wants, and the other one
   --  (Proc_Location) is a translation that is actually used to efficiently
   --  process its request. There are several kind of locations so that the
   --  user can accurately select what he wants to select.

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

   package User_Location_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => User_Location);
   subtype User_Locations is User_Location_Vectors.Vector;

   package Proc_Location_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Proc_Location);
   subtype Proc_Locations is Proc_Location_Vectors.Vector;

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

   function Translate_Location
     (Exec : Exe_File_Acc; User_Loc : User_Location) return Proc_Location;
   --  Translate a user location to a processable location using
   --  executable-specific information.

   procedure Translate_Locations
     (Exec      : Exe_File_Acc;
      User_Locs : User_Locations;
      Proc_Locs : in out Proc_Locations);
   --  Translate provided user locations and add the result to Proc_Locs.

   function Matches_Locations
     (Exec      : Exe_File_Acc;
      Locations : Proc_Locations;
      PC        : Pc_Type) return Boolean;
   --  Return whether PC (from a section in Exec) matches any location in
   --  Locations.

   function Image (L : User_Location) return String;
   --  Return a human readable string for an user location

   function Image (L : Proc_Location) return String;
   --  Return a human readable string for a processable location

   function Image (L : Address_Info_Acc) return String;
   --  Return a human readable string for a source location from debug info

end Object_Locations;
