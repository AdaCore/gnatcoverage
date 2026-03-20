package body Manual_Dump is
   procedure Dump is
   begin
      pragma Annotate (Xcov, Dump_Buffers);
   end Dump;
end Manual_Dump;
