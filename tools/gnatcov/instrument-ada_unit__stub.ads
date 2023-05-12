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

--  Stub of Instrument.Ada_Unit, to avoid pulling a dependency to libadalang
--  when gnatcov is not built with Ada instrumentation support (basically the
--  gnatcov32 executable that have support for binary traces only).

with Instrument.Common; use Instrument.Common;

package Instrument.Ada_Unit is

   pragma Elaborate_Body;

   type Ada_Instrumenter_Type is new Language_Instrumenter with null record;
   --  Common instrumentation primitives for Ada

   function Create_Ada_Instrumenter
     (Language_Version : Any_Language_Version) return Ada_Instrumenter_Type
   is (Ada_Instrumenter_Type'(null record));

   overriding function Language
     (Self : Ada_Instrumenter_Type) return Src_Supported_Language
   is (Ada_Language);

   procedure Find_Ada_Units
     (Instrumenter : in out Ada_Instrumenter_Type;
      CU_Name      : Compilation_Unit_Name;
      Info         : GNATCOLL.Projects.File_Info;
      Process_Unit : access procedure
        (CU_Name : Compilation_Unit_Name;
         Info    : GNATCOLL.Projects.File_Info)) is null;

end Instrument.Ada_Unit;
