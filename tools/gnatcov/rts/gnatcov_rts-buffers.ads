------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides types and subprograms to maintain data about the
--  satisfaction of coverage obligations.

--  This unit needs to be compilable with Ada 95 compilers

with Interfaces;
with System;
with System.Storage_Elements;

package GNATcov_RTS.Buffers is

   pragma Pure;

   --  All data takes the form of big arrays of booleans: coverage buffers. How
   --  to interpret these depend on the type of coverage obligation.
   --
   --  For statement coverage, each statement is assigned such a boolean, which
   --  indicates whether it was executed.
   --
   --  For decision coverage, each decision is assigned two booleans: one which
   --  indicates whether the decision reached the False outcome and another for
   --  the True outcome.

   type Any_Unit_Part is (Unit_Body, Unit_Spec, Unit_Separate);

   type Any_Bit_Id is new Integer;
   subtype Bit_Id is Any_Bit_Id range 0 .. Any_Bit_Id'Last;
   --  Unique identifier for a boolean in a coverage buffer

   type Coverage_Buffer_Type is array (Bit_Id range <>) of Boolean;
   --  Even though it is tempting to pack this array to save memory, we must
   --  avoid bit packing to allow concurrent writes to coverage buffers.

   type SCOs_Hash is new System.Storage_Elements.Storage_Array (1 ..  20);
   --  Hash type to perform consistency checks over Source Coverage
   --  Obligations. 20-byte to hold a SHA-1.

   type Unit_Coverage_Buffers (Unit_Name_Length : Positive) is record
      Fingerprint : SCOs_Hash;
      --  Hash of SCO info for this unit, as gnatcov computes it (see
      --  SC_Obligations). Used a fast way to check that coverage obligations
      --  and coverage data are consistent. Specific hash values are computed
      --  during instrumentation.

      Unit_Part : Any_Unit_Part;
      Unit_Name : String (1 .. Unit_Name_Length);
      --  Unit kind and name for the instrumented unit. More specifically,
      --  Unit_Name is the fully qualified name of the compilation unit (or
      --  subunit) in lower case. For instance: "foo", "ada.text_io" or
      --  "foo.bar.my_subunit".

      Statement, Decision, MCDC : System.Address;
      --  Addresses of coverage buffers for statement obligations, decision
      --  obligations and MC/DC obligations. The address refer to
      --  Coverage_Buffer_Type arrays whose bounds go from
      --  0 to Unit_Coverage_Buffers.*_Last_Bit.

      Statement_Last_Bit, Decision_Last_Bit, MCDC_Last_Bit : Any_Bit_Id;
      --  Index for the last bits in coverage buffers for statements, decisions
      --  and MC/DC.
   end record;

   -------------------------
   -- Witness subrpograms --
   -------------------------

   --  The following subprograms are called by generated code to record
   --  the execution of constructs.

   --  Note that the Buffer_Address parameters below are required so that:
   --
   --  * functions don't have IN OUT formals, which are illegal before Ada
   --    2012;
   --
   --  * our hack for pure units to be able to reference coverage buffers
   --    (which are global state, i.e. unreachable from pure units) works.

   --  Statements and declarations

   --  The following Witness subprograms set the Boolean corresponding to Bit
   --  to True in Buffer in various context.

   procedure Witness (Buffer_Address : System.Address; Bit : Bit_Id);
   --  This variant is used as part of sequence of statements, to witness
   --  that we have reached a specific point in the sequence.

   type Witness_Dummy_Type is record
      Data : Boolean;
   end record;
   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Witness_Dummy_Type;
   --  This variant is used in contexts where statements are not allowed
   --  but declarations are, for example ahead of library level declarations
   --  to witness their elaboration.

   --  Use a dummy type of minimal but non-zero size to prevent the compiler
   --  from optimizing calls away, as this unit is Pure. Use a composite type
   --  to prevent ambiguities in contexts where the Boolean variant below
   --  might be used, for example in constructs like
   --
   --    case Witness (Buffer, Bit) is
   --      when others => ...
   --    end case;
   --
   --  used for some kinds of expression functions.

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Boolean;
   --  This variant is used in Boolean expression contexts and always returns
   --  True.

   --  Decisions

   subtype MCDC_State_Type is Any_Bit_Id;

   --  When instrumenting expression functions, we need to conjure up storage
   --  for MC/DC state as limited aggregates. The following type provides for
   --  this use case.

   type MCDC_State_Holder is limited record
      State : aliased MCDC_State_Type;
   end record;

   function Witness
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean;
   --  If Value is False, set the Boolean corresponding to False_Bit to True in
   --  Buffer. Set the one corresponding to True_Bit otherwise.

   function Witness
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : Boolean) return Boolean;
   --  Same as above, and also set the bit determined by MCDC_Base and the
   --  Bit_Id value at MCDC_Path_Address in the buffer at MCDC_Buffer_Address.
   --  Note that MCDC_Path may not be passed by value, because it is not known
   --  until the side effects of the actual for the Value formal have been
   --  evaluated.

   --  Conditions

   function Witness
     (Buffer_Address  : System.Address;
      Offset_For_True : Any_Bit_Id;
      First           : Boolean;
      Value           : Boolean) return Boolean;
   --  Buffer_Address is the address of an MCDC_State_Type local variable.
   --  If First is True, first reset it to 0.
   --  If Value is True, add Offset_For_True.

end GNATcov_RTS.Buffers;
