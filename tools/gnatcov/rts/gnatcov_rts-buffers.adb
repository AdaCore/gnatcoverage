------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

--  This unit needs to be compilable with Ada 95 compilers

package body GNATcov_RTS.Buffers is

   function To_Bool (Bool : GNATcov_RTS_Bool) return Boolean;

   function To_GNATcov_RTS_Bool (Bool : Boolean) return GNATcov_RTS_Bool;

   function GNATcov_RTS_Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return GNATcov_RTS_Bool;
   pragma Import (C, GNATcov_RTS_Witness);

   function GNATcov_RTS_Witness_Decision
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Bit_Id) return GNATcov_RTS_Bool;
   pragma Import (C, GNATcov_RTS_Witness_Decision);

   function GNATcov_RTS_Witness_Decision_MCDC
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : GNATcov_RTS_Bool) return GNATcov_RTS_Bool;
   pragma Import (C, GNATcov_RTS_Witness_Decision_MCDC);

   function GNATcov_RTS_Witness_Condition
     (MCDC_Path_Address : System.Address;
      Offset_For_True   : Any_Bit_Id;
      First             : GNATcov_RTS_Bool;
      Value             : GNATcov_RTS_Bool) return GNATcov_RTS_Bool;
   pragma Import (C, GNATcov_RTS_Witness_Condition);

   -------------
   -- To_Bool --
   -------------

   function To_Bool (Bool : GNATcov_RTS_Bool) return Boolean is
   begin
      return Bool /= 0;
   end To_Bool;

   -------------------------
   -- To_GNATcov_RTS_bool --
   -------------------------

   function To_GNATcov_RTS_Bool (Bool : Boolean) return GNATcov_RTS_Bool is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_GNATcov_RTS_Bool;

   -------------
   -- Witness --
   -------------

   procedure Witness (Buffer_Address : System.Address; Bit : Bit_Id) is
      Ignored : GNATcov_RTS_Bool :=
        GNATcov_RTS_Witness (Buffer_Address, Bit);
   begin
      null;
   end Witness;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Witness_Dummy_Type
   is
   begin
      Witness (Buffer_Address, Bit);
      return (Data => False);
   end Witness;

   function Witness
     (Buffer_Address : System.Address; Bit : Bit_Id) return Boolean
   is
   begin
      Witness (Buffer_Address, Bit);
      return False;
   end Witness;

   function Witness
     (Buffer_Address      : System.Address;
      False_Bit, True_Bit : Bit_Id;
      Value               : Boolean) return Boolean is
   begin
      if Value then
         Witness (Buffer_Address, True_Bit);
      else
         Witness (Buffer_Address, False_Bit);
      end if;
      return Value;
   end Witness;

   function Witness
     (Decision_Buffer_Address : System.Address;
      False_Bit, True_Bit     : Bit_Id;
      MCDC_Buffer_Address     : System.Address;
      MCDC_Base               : Bit_Id;
      MCDC_Path_Address       : System.Address;
      Value                   : Boolean) return Boolean
   is
   begin
      return
        To_Bool
          (GNATcov_RTS_Witness_Decision_MCDC
             (Decision_Buffer_Address, False_Bit, True_Bit,
              MCDC_Buffer_Address, MCDC_Base, MCDC_Path_Address,
              To_GNATcov_RTS_Bool (Value)));
   end Witness;

   function Witness
     (Buffer_Address  : System.Address;
      Offset_For_True : Any_Bit_Id;
      First           : Boolean;
      Value           : Boolean) return Boolean
   is
   begin
      return
        To_Bool
          (GNATcov_RTS_Witness_Condition
             (Buffer_Address, Offset_For_True, To_GNATcov_RTS_Bool (First),
              To_GNATcov_RTS_Bool (Value)));
   end Witness;

end GNATcov_RTS.Buffers;
