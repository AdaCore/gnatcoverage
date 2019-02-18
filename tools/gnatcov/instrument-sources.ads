------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  Generate SCOs and source code instrumentation

with Libadalang.Analysis;     use Libadalang.Analysis;
with Libadalang.Rewriting;    use Libadalang.Rewriting;

with Instrument.Common; use Instrument.Common;
with Strings;

private package Instrument.Sources is

   type Rewriting_Material is record
      Common_Buffers : Node_Rewriting_Handle;
      --  Qualified name for the unit that contains coverage buffer types and
      --  witness subprograms.

      Unit_Buffers : Node_Rewriting_Handle;
      --  Qualified name for the unit that contains coverage buffers

      Stmt_Buffer : Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to statement coverage
      --  obligations.

      Dc_Buffer : Node_Rewriting_Handle;
      --  Qualified name for the buffer corresponding to decision coverage
      --  obligations.
   end record;

   -----------------------------
   -- Instrumentation context --
   -----------------------------

   --  This is the global state for the process of instrumenting a compilation
   --  unit.

   type Unit_Inst_Context is record
      Instrumented_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit currently being instrumented

      Buffer_Unit : Compilation_Unit_Name;
      --  Name of the compilation unit that holds coverage buffers for the
      --  unit currently being instrumented.

      RH_Ctx : Rewriting_Handle;
      --  Rewriting handle for the instrumentation process

      Unit_Bits : LL_Unit_Bit_Maps;

      Material : Rewriting_Material;
      --  Bank of nodes to use during instrumentation
   end record;

   procedure Create_Rewriting_Material (Context : in out Unit_Inst_Context);
   --  Create nodes in Context.Material

   procedure Instrument_Unit
     (CU_Name  : Compilation_Unit_Name;
      Filename : String;
      IC       : Inst_Context;
      UIC      : out Unit_Inst_Context);
   --  Generate the instrumented source corresponding to CU_Name, a unit whose
   --  source file is Filename. Record instrumentation information in IC.

   function Img (Bit : Any_Bit_Id) return String is
     (Strings.Img (Integer (Bit)));

end Instrument.Sources;
