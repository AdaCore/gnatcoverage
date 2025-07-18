pragma Ada_2022;

procedure Test_1 is
   function Foo return Integer is (42) with Static; -- # expr
begin
   null; -- # stmt
end Test_1;

--# test_1.adb
--
--  /expr/ l. ## 0
--  /stmt/ l+ ## 0
