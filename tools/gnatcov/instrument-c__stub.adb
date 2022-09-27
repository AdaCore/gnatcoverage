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

package body Instrument.C is

   Error_Msg : constant String :=
     "GNATcoverage not built with C support.";

   ----------------------
   -- Skip_Source_File --
   ----------------------

   function Skip_Source_File
     (Self        : C_Family_Instrumenter_Type;
      Source_File : GNATCOLL.Projects.File_Info) return Boolean
   is (raise Program_Error with Error_Msg);

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit
     (Self      : C_Family_Instrumenter_Type;
      CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info) is
   begin
      raise Program_Error with Error_Msg;
   end Instrument_Unit;

   -------------------------------
   -- Auto_Dump_Buffers_In_Main --
   -------------------------------

   procedure Auto_Dump_Buffers_In_Main
     (Self     : C_Family_Instrumenter_Type;
      IC       : in out Inst_Context;
      Main     : Compilation_Unit_Name;
      Filename : String;
      Info     : in out Project_Info) is
   begin
      raise Program_Error with Error_Msg;
   end Auto_Dump_Buffers_In_Main;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   procedure Emit_Buffers_List_Unit
     (Self              : C_Family_Instrumenter_Type;
      IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info) is
   begin
      raise Program_Error with Error_Msg;
   end Emit_Buffers_List_Unit;

end Instrument.C;
