with Foo;

procedure Test_Part is
begin
   Foo (False, False);
   Foo (True, False);
end Test_Part;

--# foo.adb
--
--  /stmt/ l+ ## 0
--  /dec/  l+ ## 0
--  /mcdc/ l! ## c!
