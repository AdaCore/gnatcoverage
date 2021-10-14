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

   ---------------------------
   -- Add_Auto_Dump_Buffers --
   ---------------------------

   procedure Add_Auto_Dump_Buffers
     (IC   : Inst_Context;
      Info : in out Project_Info;
      Main : Compilation_Unit_Name;
      Rew  : C_Source_Rewriter) is
   begin
      raise Program_Error with Error_Msg;
   end Add_Auto_Dump_Buffers;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out C_Source_Rewriter) is
   begin
      raise Program_Error with Error_Msg;
   end Apply;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self           : out C_Source_Rewriter;
      Info           : in out Project_Info;
      Input_Filename : String) is
   begin
      raise Program_Error with Error_Msg;
   end Start_Rewriting;

   ----------------------------
   -- Emit_Buffers_List_Unit --
   ----------------------------

   procedure Emit_Buffers_List_Unit
     (IC                : in out Inst_Context;
      Root_Project_Info : in out Project_Info) is
   begin
      raise Program_Error with Error_Msg;
   end Emit_Buffers_List_Unit;

   ---------------------
   -- Instrument_Unit --
   ---------------------

   procedure Instrument_Unit
     (CU_Name   : Compilation_Unit_Name;
      IC        : in out Inst_Context;
      Unit_Info : in out Instrumented_Unit_Info) is
   begin
      raise Program_Error with Error_Msg;
   end Instrument_Unit;

end Instrument.C;
