------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

--  Stub of Instrument.C, to avoid pulling a dependency to libclang when
--  gnatcov is not built with C support.
--
--  TODO??? this will need rework when C is enabled by default (see V222-037).
--  When gnatcov is built with C_SUPPORT=False, it will try to instrument C
--  units, using the procedures defined here, which will result in a crash (all
--  the procedures are empty stubs raising an error).

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Instrument.Base_Types; use Instrument.Base_Types;
with Instrument.Common;     use Instrument.Common;
with Switches;              use Switches;

private package Instrument.C is

   type C_Family_Instrumenter_Type is
     abstract new Language_Instrumenter with null record;
   --  Common instrumentation primitives for C/C++

   overriding function Skip_Source_File
     (Self        : C_Family_Instrumenter_Type;
      Source_File : GNATCOLL.Projects.File_Info) return Boolean;

   overriding procedure Instrument_Unit
     (Self      : C_Family_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);

   overriding procedure Auto_Dump_Buffers_In_Main
     (Self     : C_Family_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info);

   overriding procedure Emit_Buffers_List_Unit
     (Self              : C_Family_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);

   type C_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C

   overriding function Language
     (Self : C_Instrumenter_Type) return Src_Supported_Language
   is (C_Language);

   type CPP_Instrumenter_Type is
     new C_Family_Instrumenter_Type with null record;
   --  Instrumentation primitives for C++

   overriding function Language
     (Self : CPP_Instrumenter_Type) return Src_Supported_Language
   is (CPP_Language);

   C_Instrumenter   : aliased constant C_Instrumenter_Type := (null record);
   CPP_Instrumenter : aliased constant CPP_Instrumenter_Type := (null record);

end Instrument.C;
