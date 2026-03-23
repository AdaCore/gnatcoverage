with Foo; use Foo;
with Assert;

procedure Test_FF is
begin
   Foo.Try (V => 4);
   Assert (N_Then = 0);
   Assert (N_Else = 1);
end;

--# foo.adb
--  /eval/ l! ## dT-
--  /then/ l- ## s-
--  /else/ l+ ## 0
