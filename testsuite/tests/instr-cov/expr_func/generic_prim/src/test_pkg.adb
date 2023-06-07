pragma Ada_2012;

with Inst;

procedure Test_Pkg is

   Res : Boolean with Volatile;

begin
   Res := Inst.Obj.Foo;
end Test_Pkg;

--  The coverage expectations do not matter much, the point of this test is to
--  check that the instrumented code is accepted by gnat.

--# pkg.ads
--
-- /expr/ l+ ## 0
