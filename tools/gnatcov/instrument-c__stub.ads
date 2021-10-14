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

with Instrument.Base_Types; use Instrument.Base_Types;
with Instrument.Common;     use Instrument.Common;

private package Instrument.C is
   type C_Source_Decision is null record;

   type C_Source_Condition is null record;

   type C_Unit_Inst_Context is null record;

   type C_Source_Rewriter is tagged limited null record;

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : C_Source_Rewriter);

   procedure Apply (Self : in out C_Source_Rewriter);

   procedure Start_Rewriting
     (Self           : out C_Source_Rewriter;
      Info           : in out Project_Info;
      Input_Filename : String);

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info);

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info);

end Instrument.C;
