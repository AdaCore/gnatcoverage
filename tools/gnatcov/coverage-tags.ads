------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with Factory_Registry;
with Slocs;            use Slocs;
with Traces;           use Traces;
with Traces_Elf;       use Traces_Elf;
with Traces_Names;     use Traces_Names;

package Coverage.Tags is

   --------------------------
   -- Source coverage tags --
   --------------------------

   --  In some contexts, several distinct coverage analyses must be conducted
   --  in parallel for a given SCO (e.g. when separately assessing coverage on
   --  different instances of the same generic unit). In such cases, each
   --  separate analysis is assigned a specific tag. The special value
   --  No_SC_Tag denotes the default cumulative analysis made for a SCO when
   --  no such distinction exists.

   type Tag_Provider_Type is abstract tagged limited record
      Current_Routine : Subprogram_Info;
      Current_Subp    : Address_Info_Acc;
   end record;

   type Tagged_Sloc is record
      Sloc : Source_Location;
      Tag  : SC_Tag;
   end record;

   type Tagged_Slocs is array (Positive range <>) of Tagged_Sloc;
   --  Note: must have the same index base subtype as Slocs.Source_Locations

   function Get_Slocs_And_Tags
     (TP : access Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs is abstract;
   --  Return a list of (sloc; tag) pairs for the given executable location.
   --  Note that for PC that is associated with more than one sloc (i.e. more
   --  than one SCO), the relevant tag may be different for each sloc/SCO.
   --  Enter_Routine must have been called previously to identify the
   --  enclosing subprogram; code outside of a subprogram is defined to
   --  always have No_SCO_Tag.

   function Tag_Name
     (TP  : access Tag_Provider_Type;
      Tag : SC_Tag) return String is abstract;
   --  Return a user readable name for the given tag

   procedure Enter_Routine
     (TP        : access Tag_Provider_Type;
      Subp_Info : Traces_Names.Subprogram_Info);
   --  Record Subp_Info as the subprogram information for the routine being
   --  analyzed (sets TP.Current_Routine).

   package Tag_Providers is
     new Factory_Registry (Tag_Provider_Type);

   subtype Tag_Provider_Access is Tag_Providers.RT_Access;

   ----------------------------
   -- Default tag repository --
   ----------------------------

   --  This repository does not assign any tags

   type Default_Tag_Provider_Type is new Tag_Provider_Type with private;

   overriding function Get_Slocs_And_Tags
     (TP : access Default_Tag_Provider_Type;
      PC : Pc_Type) return Tagged_Slocs;
   --  Return all slocs for PC tagged with No_SC_Tag

   overriding function Tag_Name
     (TP  : access Default_Tag_Provider_Type;
      Tag : SC_Tag) return String;
   --  Return the empty string

   Tag_Provider : Tag_Provider_Access;
   Default_Tag_Provider_Name : constant String := "default";

   ----------------------
   -- Utility routines --
   ----------------------

   function Get_Slocs_With_Tag
     (Set : Address_Info_Sets.Set;
      PC  : Pc_Type;
      Tag : SC_Tag) return Tagged_Slocs;
   --  Return all slocs for PC, tagged with Tag

private

   type Default_Tag_Provider_Type is new Tag_Provider_Type with null record;

   package R is new Tag_Providers.Register_Factory
     (Name => Default_Tag_Provider_Name,
      T    => Default_Tag_Provider_Type);

end Coverage.Tags;
