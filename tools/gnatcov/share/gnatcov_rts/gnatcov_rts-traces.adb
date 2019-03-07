--  This unit needs to be compilable with Ada 95 compilers

with System;

package body GNATcov_RTS.Traces is

   ----------------------
   -- Native_Endianity --
   ----------------------

   function Native_Endianity return Supported_Endianity is
      use type System.Bit_Order;
   begin
      if System.Default_Bit_Order = System.Low_Order_First then
         return Little_Endian;
      else
         return Big_Endian;
      end if;
   end Native_Endianity;

end GNATcov_RTS.Traces;
