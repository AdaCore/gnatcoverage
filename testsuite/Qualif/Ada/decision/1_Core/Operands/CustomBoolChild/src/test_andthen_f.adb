with Support, Par.Andthen; use Support, Par.Andthen;

procedure Test_Andthen_F is
begin
   Assert (And_Then_Custom (False, True) = False);
   Assert (And_Then_Custom (True, False) = False);
end;

--# par-andthen.adb
--  /call/  l+ ## 0
--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
--  /mkc/   l- ## s-
