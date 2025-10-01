------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with GPR2;
with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;

--  Custom implementation of a libadalang unit provider, using the mapping
--  file produced from the project file (that is passed to the compiler through
--  the -gnatem switch).

package Instrument.Ada_Unit_Provider is

   procedure Create_Mapping_File (Filename : String);
   --  Write a mapping for all Ada sources in the currently loaded project.
   --  This mapping file is intended to be passed as Dependencies_Filename to
   --  Create_Provider.

   type Provider_Type is
     new Libadalang.Analysis.Unit_Provider_Interface with private;

   function Create_Provider (Mapping_File : String) return Provider_Type;
   --  Create a unit provider from a mapping file in the same format as the
   --  file passed through -gnatem in the compiler invocation.

   overriding
   function Get_Unit_Filename
     (Provider : Provider_Type; Name : Text_Type; Kind : Analysis_Unit_Kind)
      return String;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   overriding
   function Get_Unit
     (Provider : Provider_Type;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   overriding
   procedure Release (Provider : in out Provider_Type) is null;

   function Has_Unit
     (Provider  : Provider_Type;
      Unit_Name : String;
      Unit_Part : GPR2.Valid_Unit_Kind) return Boolean;
   --  Returns whether given unit is in the provider unit closure

private

   package String_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   type Provider_Type is new Libadalang.Analysis.Unit_Provider_Interface
   with record
      Unit_Map : String_Maps.Map;
      --  Mapping from unit name to file fullnames
   end record;

end Instrument.Ada_Unit_Provider;
