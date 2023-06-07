with Pkg;
with Pkh;

procedure Main is
   function C_Foo (I : Integer) return Integer;
   pragma Import (C, C_Foo, "foo");

   Dummy : Integer := C_Foo (1);
begin
   Pkg.Say_Hello;
   Pkg.Test;
   Pkh.Say_Hello;
   Pkh.Test;
end Main;
