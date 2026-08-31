pragma Style_Checks (Off); pragma Warnings (Off);

with Interfaces;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package body GCVRT.FOO.Observe is
   function Sum_Buffer_Bits return Natural is
      function Sum_Buffer_Bits_C
        (C_Arrays_List : GNATcov_RTS_Coverage_Buffers_Group_Array_List)
         return Interfaces.Unsigned_64;
      pragma Import (C, Sum_Buffer_Bits_C, "gnatcov_rts_sum_buffer_bits_list");
   begin
      return
        Natural
          (Interfaces.Unsigned_64'Min
             (Sum_Buffer_Bits_C (GCVRT.FOO.C_Arrays_List),
              Interfaces.Unsigned_64 (Natural'Last)));
   end Sum_Buffer_Bits;
end GCVRT.FOO.Observe;
