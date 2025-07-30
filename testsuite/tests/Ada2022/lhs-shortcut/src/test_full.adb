with Foo;

procedure Test_Full is
begin
   Foo (False, False);
   Foo (True, False);
   Foo (False, True);
end Test_Full;

--# foo.adb
--
--  /stmt/ l+ ## 0
--  /dec/  l+ ## 0
--  /mcdc/ l+ ## 0
