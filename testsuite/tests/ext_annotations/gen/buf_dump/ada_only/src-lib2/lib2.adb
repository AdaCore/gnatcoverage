package body Lib2 is
   function Bar return Integer
   is
      Res : constant Integer := 1;
   begin
      pragma Annotate (Xxxx, Dump_Buffers);
      return Res;
   end Bar;
end Lib2;
