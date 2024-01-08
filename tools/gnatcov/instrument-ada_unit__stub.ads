------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Stub of Instrument.Ada_Unit: see the note about Stubs in gnatcov.gpr

with Instrument.Common; use Instrument.Common;

package Instrument.Ada_Unit is

   pragma Elaborate_Body;

   type Ada_Instrumenter_Type is new Language_Instrumenter with null record;
   --  Common instrumentation primitives for Ada

   overriding function Language
     (Self : Ada_Instrumenter_Type) return Src_Supported_Language
   is (Ada_Language);

   function Create_Ada_Instrumenter
     (Tag                        : Unbounded_String;
      Config_Pragmas_Filename,
      Mapping_Filename           : String;
      Predefined_Source_Dirs     : String_Vectors.Vector;
      Preprocessor_Data_Filename : String)
      return Ada_Instrumenter_Type
   is (Ada_Instrumenter_Type'(others => <>));

end Instrument.Ada_Unit;
