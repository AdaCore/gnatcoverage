pragma Ada_2012;

with Foo; use Foo;

--  Check that function coverage violations are correctly reported for a
--  function, an expression function, a procedure and a null procedure inside
--  a protected body.

procedure Test_Not_Called is
begin
   null;
end Test_Not_Called;

--# foo.ads
-- /decl/      l+ ## 0
--# foo.adb
-- /fun/       l- ## f-
-- /expr_fun/  l- ## s=>s-, f=>s-,f-
-- /call_stmt/ f=>l- ## s=>s-, f=>s-,c-
-- /stmt/      l- ## s-
