with Lib2;

package body Lib1 is
   function Foo return Integer
   is
      Res : Integer := 1;
   begin
      Res := Res + Lib2.Bar;
      pragma Annotate (Xcov, Dump_Buffers);
      return Res;
   end Foo;
end Lib1;
