pragma Ada_2012;

with Foo; use Foo;

--  Check that function coverage violations are correctly reported for a
--  function, an expression function, a procedure and a null procedure inside
--  a protected body.

procedure Test_Call
is
   I : Integer := 42;
begin
   I := Obj.F;
   if Obj.E (I) then
      Obj.P ("Hello");
   end if;
   Obj.NP;
end Test_Call;

--# foo.ads
-- /decl/      l+ ## 0
--# foo.adb
-- /fun/       l+ ## 0
-- /expr_fun/  l+ ## 0
-- /call_stmt/ l+ ## 0
-- /stmt/      l+ ## 0
