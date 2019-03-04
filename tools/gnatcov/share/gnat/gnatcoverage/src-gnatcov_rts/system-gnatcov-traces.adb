package body System.GNATcov.Traces is

   ----------------------
   -- Native_Endianity --
   ----------------------

   function Native_Endianity return Supported_Endianity is
   begin
      if Default_Bit_Order = Low_Order_First then
         return Little_Endian;
      else
         return Big_Endian;
      end if;
   end Native_Endianity;

end System.GNATcov.Traces;
