package body Lib1 is
   function Foo return Integer
   is
      Res : constant Integer := 1;
   begin
      pragma Annotate (Xxxx, Dump_Buffers);
      return Res;
   end Foo;
end Lib1;
