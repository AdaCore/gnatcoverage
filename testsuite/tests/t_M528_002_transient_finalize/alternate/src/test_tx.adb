with Foo; use Foo;
with Assert;

procedure Test_TX is
begin
   Foo.Try (V => 1);
   Assert (N_Then = 1);
   Assert (N_Else = 0);
end;

--# foo.adb
--  /eval/ l! ## dF-
--  /then/ l+ ## 0
--  /else/ l- ## s-
