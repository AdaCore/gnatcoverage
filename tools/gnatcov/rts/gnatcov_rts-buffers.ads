------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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
--  satisfaction of coverage obligations. All the functions are Ada bindings
--  to the functions defined in gnatcov_rts_c-buffers.h.
--
--  This unit needs to be compilable with Ada 95 compilers.

with System;

with GNATcov_RTS.Types;   use GNATcov_RTS.Types;
with GNATcov_RTS.Strings; use GNATcov_RTS.Strings;

package GNATcov_RTS.Buffers is

   pragma Pure;
   pragma Warnings (Off);
   pragma No_Elaboration_Code_All;
   pragma Warnings (On);

   --  Any_Bit_Id is declared volatile here, as in order to instrument declare
   --  expressions we need to declare a constant MC_DC_State (a subtype of
   --  Any_Bit_Id) object, but prevent the compiler from optimizing out the
   --  variable.

   type Any_Bit_Id is new int;
   pragma Volatile (Any_Bit_Id);

   No_Bit_Id : constant Any_Bit_Id := -1;

   subtype Bit_Id is Any_Bit_Id range 0 .. Any_Bit_Id'Last;
   --  Unique identifier for a boolean in a coverage buffer

   type Coverage_Buffer_Type is array (Bit_Id range <>) of Boolean;
   --  Even though it is tempting to pack this array to save memory, we must
   --  avoid bit packing to allow concurrent writes to coverage buffers.

   Fingerprint_Size : constant := 20;

   type GNATcov_RTS_Bool is new unsigned;

   type Any_Unit_Part is
     (Not_Applicable_Part, Unit_Body, Unit_Spec, Unit_Separate);
   pragma Convention (C, Any_Unit_Part);

   type Any_Language_Kind is (Unit_Based_Language, File_Based_Language);
   pragma Convention (C, Any_Language_Kind);

   type Fingerprint_Type is array (1 .. 20) of Unsigned_8;
   for Fingerprint_Type'Component_Size use 8;
   for Fingerprint_Type'Size use 20 * 8;

   --  Hash type to perform consistency checks over Source Coverage
   --  Obligations. 20-byte to hold a SHA-1.

   type GNATcov_RTS_Coverage_Buffers is record
      Fingerprint                                          : Fingerprint_Type;
      Language                                             : Any_Language_Kind;
      Unit_Part                                            : Any_Unit_Part;
      Unit_Name                                            :
        GNATcov_RTS_String;
      Bit_Maps_Fingerprint                                 : Fingerprint_Type;
      Annotations_Fingerprint                              : Fingerprint_Type;
      Statement, Decision, MCDC                            : System.Address;
      Statement_Last_Bit, Decision_Last_Bit, MCDC_Last_Bit : Any_Bit_Id;
   end record;
   pragma Convention (C, GNATcov_RTS_Coverage_Buffers);

   -------------------------
   -- Witness subprograms --
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
   pragma Inline (Witness);
   --  This variant is used as part of sequence of statements, to witness
   --  that we have reached a specific point in the sequence.

   type Witness_Dummy_Type is record
      Data : Boolean;
   end record;
   pragma Volatile (Witness_Dummy_Type);

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Witness_Dummy_Type;
   pragma Inline (Witness);
   --  This variant is used in contexts where statements are not allowed
   --  but declarations are, for example ahead of library level declarations
   --  to witness their elaboration.

   --  Use a volatile dummy type of minimal but non-zero size to prevent the
   --  compiler from optimizing calls away, as this unit is Pure. Use a
   --  composite type to prevent ambiguities in contexts where the Boolean
   --  variant below might be used, for example in constructs like
   --
   --    case Witness (Buffer, Bit) is
   --      when others => ...
   --    end case;
   --
   --  used for some kinds of expression functions.

   type Non_Volatile_Witness_Dummy_Type is record
      Data : Boolean;
   end record;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id)
      return Non_Volatile_Witness_Dummy_Type;
   pragma Inline (Witness);
   --  This variant uses a non-volatile return type to be compatible with ghost
   --  code.

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Boolean;
   pragma Inline (Witness);
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
   pragma Inline (Witness);
   --  If Value is False, set the Boolean corresponding to False_Bit to True in
   --  Buffer. Set the one corresponding to True_Bit otherwise.

   function Witness
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : Boolean) return Boolean;
   pragma Inline (Witness);
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
   pragma Inline (Witness);
   --  Buffer_Address is the address of an MCDC_State_Type local variable.
   --  If First is True, first reset it to 0.
   --  If Value is True, add Offset_For_True.

end GNATcov_RTS.Buffers;
