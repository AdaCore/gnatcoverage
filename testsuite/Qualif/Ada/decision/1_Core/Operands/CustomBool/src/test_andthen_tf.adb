with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_TF is
begin
   Assert (And_Then_Custom (True, False) = False);
end;

--# andthen.adb
--  /call/  l+ ## 0
--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
--  /mkc/   l- ## s-
