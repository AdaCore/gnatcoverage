with Lib3;

package body Lib1 is
   function Foo return Integer
   is
      Res : Integer := 1;
   begin
      Res := Res + Lib3.Baz;
      pragma Annotate (Xcov, Dump_Buffers);
      return Res;
   end Foo;
end Lib1;
