package body Pkg is

   procedure Do_Dump is
   begin
      null;
      pragma Annotate (Xcov, Dump_Buffers);
   end Do_Dump;

end Pkg;
