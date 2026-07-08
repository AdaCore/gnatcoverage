------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
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

with Instrument.Actions.Instrument_Source.Ada;

package body Instrument.Actions.Instrument_Source.Process is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Object;
      LU_Info      : Library_Unit_Info;
      IC           : Inst_Context_Acc;
      Instrumenter : Instrument.Common.Language_Instrumenter_Acc;
      Prj_Info     : Project_Info_Access;
      Dump_Config  : Any_Dump_Config) is
   begin
      Self.Set_View (LU_Info.Instr_Project);
      if LU_Info.Language = Switches.Ada_Language then
         Self.Instrument_Object :=
           new Instrument.Actions.Instrument_Source.Ada.Object;
      elsif LU_Info.Language in Switches.C_Language | Switches.CPP_Language
      then
         Self.Instrument_Object :=
           new Instrument.Actions.Instrument_Source.Object;
      else
         raise Program_Error;
      end if;
      Self.Instrument_Object.Initialize
        (LU_Info, IC, Instrumenter, Prj_Info, Dump_Config);
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      pragma Unreferenced (Db);
   begin
      return True;
   end On_Tree_Insertion;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean)
   is
      Signature : GPR2.Build.Signature.Object := Self.Signature;
   begin
      Self.Instrument_Object.Compute_Signature (Signature, Check_Checksums);
      Self.Set_Signature (Signature);
   end Compute_Signature;

   ---------------------
   -- Compute_Command --
   ---------------------

   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean) is
   begin
      Cmd_Line := Self.Instrument_Object.Compute_Command;
   end Compute_Command;

   --------------------
   -- Post_Execution --
   --------------------

   overriding
   function Post_Execution
     (Self   : in out Object;
      Status : GPR2.Build.Actions.Execution_Status;
      Stdout : US.Unbounded_String := US.Null_Unbounded_String;
      Stderr : US.Unbounded_String := US.Null_Unbounded_String) return Boolean
   is
   begin
      return Self.Instrument_Object.Post_Execution (Status, Stdout, Stderr);
   end Post_Execution;

end Instrument.Actions.Instrument_Source.Process;
