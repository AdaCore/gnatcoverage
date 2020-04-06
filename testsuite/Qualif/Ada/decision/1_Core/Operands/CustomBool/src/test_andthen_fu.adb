with Support, Andthen ; use Support, Andthen;

procedure Test_Andthen_FU is
begin
   Assert (And_Then_Custom (True, True) = True);
   Assert (And_Then_Custom (True, False) = False);
   Assert (And_Then_Custom (False, True) = False);
end;

--# andthen.adb
--  /call/  l+ ## 0
--  /eval/  l+ ## 0
--  /true/  l+ ## 0
--  /false/ l+ ## 0
--  /mkc/   l- ## s-
