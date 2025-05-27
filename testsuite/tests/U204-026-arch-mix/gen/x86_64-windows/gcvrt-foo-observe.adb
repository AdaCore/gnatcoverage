with Interfaces;
with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

package body GCVRT.Foo.Observe is
   function Sum_Buffer_Bits return Natural is
      function Sum_Buffer_Bits_C
        (C_List : GNATcov_RTS_Coverage_Buffers_Group_Array)
      return Interfaces.Unsigned_64;
pragma Import (C, Sum_Buffer_Bits_C, "gnatcov_rts_sum_buffer_bits");
   begin
      return Natural
        (Interfaces.Unsigned_64'Min
           (Sum_Buffer_Bits_C (GCVRT.Foo.C_List),
            Interfaces.Unsigned_64 (Natural'Last)));
   end;
end GCVRT.Foo.Observe;
