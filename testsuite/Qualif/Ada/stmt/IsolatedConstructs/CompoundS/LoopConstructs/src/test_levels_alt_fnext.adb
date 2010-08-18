with Support, Levels.ALT; use Support, Levels, Levels.ALT;

--P Exercize All_LT False from follower of first in sample

procedure Test_Levels_ALT_Fnext is
   S : Sample := (1, 5);
begin
   Assert (not All_LT (2, S));
end;

--# levels-alt.adb
-- /decl/         l+ 0
-- /preLoop/      l+ 0
-- /indexCheck/   l+ 0
-- /preValCheck/  l+ 0
-- /valueCheck/   l+ 0
-- /postValCheck/ l+ 0
-- /postLoop/     l+ 0

