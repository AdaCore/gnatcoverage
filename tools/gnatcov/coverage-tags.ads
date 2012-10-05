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

with Traces;     use Traces;
with Traces_Elf; use Traces_Elf;

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

   type Tag_Repository_Type is abstract tagged limited private;
   type Tag_Repository_Access is access all Tag_Repository_Type'Class;

   function Get_Tag
     (TR  : access Tag_Repository_Type;
      Exe : Exe_File_Acc;
      PC  : Pc_Type) return SC_Tag
      is abstract;
   --  Return the tag for the given executable location

   function Tag_Name
     (TR  : access Tag_Repository_Type;
      Tag : SC_Tag) return String is abstract;
   --  Return a user readable name for the given tag

   ----------------------------
   -- Default tag repository --
   ----------------------------

   --  This repository does not assign any tags

   type Default_Tag_Repository_Type is new Tag_Repository_Type with private;

   overriding function Get_Tag
     (TR  : access Default_Tag_Repository_Type;
      Exe : Exe_File_Acc;
      PC  : Pc_Type) return SC_Tag;
   --  Return No_SC_Tag always

   overriding function Tag_Name
     (TR  : access Default_Tag_Repository_Type;
      Tag : SC_Tag) return String;
   --  Return an empty string

   Tag_Repository : Tag_Repository_Access;

private

   type Tag_Repository_Type is abstract tagged limited null record;

   type Default_Tag_Repository_Type is
     new Tag_Repository_Type with null record;

   Default_Tag_Repository : aliased Default_Tag_Repository_Type;

end Coverage.Tags;
