with Support, Par.Andthen; use Support, Par.Andthen;

procedure Test_Andthen_T is
begin
   Assert (And_Then_Custom (True, True) = True);
end;

--# par-andthen.adb
--  /call/  l+ ## 0
--  /eval/  l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-
--  /mkc/   l- ## s-
