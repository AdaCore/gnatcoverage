------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

with GNATCOLL.Projects;

with Instrument.Common; use Instrument.Common;

procedure Instrument.Find_Units
  (IC           : in out Inst_Context;
   CU_Name      : Compilation_Unit_Name;
   Info         : GNATCOLL.Projects.File_Info;
   Process_Unit : access procedure
     (CU_Name : Compilation_Unit_Name;
      Info    : GNATCOLL.Projects.File_Info));
--  Consider that Info is a source file to instrument (i.e. a unit of interest,
--  CU_Name being the name of its compilation unit) and call Process_Unit for
--  all compilation units that must be instrumented with it (i.e. related
--  subunits, if present).
