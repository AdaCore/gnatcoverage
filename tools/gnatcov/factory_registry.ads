------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Ada.Tags;

generic
   type RT is abstract tagged limited private;
   --  Root abstract type of the factory

package Factory_Registry is

   --------------------------------------------
   -- Generic tagged object factory registry --
   --------------------------------------------

   --  For a given root abstract type, this generic provides a map of names
   --  to factories for derived types. For each derived type, the factory
   --  is created and registered by instantiating the Register_Factory nested
   --  generic.

   type RT_Access is access all RT'Class;
   type Create_Function is access function return RT_Access;

   --  Registration package, to be instantiated for each derived type of RT

   generic
      Name : String;
      type T is new RT with private;
   package Register_Factory is
   private
      function Create_T return RT_Access;
      Create_T_Acc : constant Create_Function := Create_T'Access;
   end Register_Factory;

   function Create (Name : String) return RT_Access;
   --  Return an object of the derived type of RT registered with the given
   --  Name. Raises Constraint_Error if no such RT was found.

   function Name (Tag : Ada.Tags.Tag) return String;
   --  Return the name of the object factory associated with the given tag.
   --  Raises Constraint_Error if not found.

   function Registered_Names (Separator : String) return String;
   --  Return a list of names for registered derived types, separated using
   --  Separator.

end Factory_Registry;
