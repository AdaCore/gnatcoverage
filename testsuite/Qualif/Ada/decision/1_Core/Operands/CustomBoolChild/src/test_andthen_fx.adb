with Support, Par.Andthen; use Support, Par.Andthen;

procedure Test_Andthen_FX is
begin
   for X in False .. True loop
      Assert (And_Then_Custom (False, Make_Custom (X)) = False);
   end loop;
end;

--# par-andthen.adb
--  /call/  l+ ## 0
--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
--  /mkc/   l+ ## 0
