with Support, Levels.ALT; use Support, Levels, Levels.ALT;

--P Check behavior when no functional code is called.

procedure Test_Levels_ALT_0 is
begin
   null;
end;

--# levels-alt.adb
-- /decl/ l- s-
-- /(preLoop|indexCheck|preValCheck|valueCheck|postValCheck|postLoop)/ l- s-

