with Foo;
with Log;

procedure Test_Foo is

begin
   Log (Foo.Bar (110, 70));
end Test_Foo;

--# foo.ads
--  =/decision-1/ l! ## dF-
--  =/decision-2/ l- ## 0
