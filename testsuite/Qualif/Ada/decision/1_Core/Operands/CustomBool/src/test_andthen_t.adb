with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_T is
begin
   Assert (And_Then_Custom (True, True) = True);
end;

--# andthen.adb
--  /call/  l+ ## 0
--  /eval/  l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-
--  /mkc/   l- ## s-
