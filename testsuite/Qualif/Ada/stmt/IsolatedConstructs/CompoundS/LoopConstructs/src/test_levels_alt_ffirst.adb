with Support, Levels.ALT; use Support, Levels, Levels.ALT;

--P Exercize All_LT False from first in sample

procedure Test_Levels_ALT_Ffirst is
   S : Sample := (5, 2);
begin
   Assert (not All_LT (1, S));
end;

--# levels-alt.adb
-- /decl/         l+ 0
-- /preLoop/      l+ 0
-- /indexCheck/   l+ 0
-- /preValCheck/  l+ 0
-- /valueCheck/   l+ 0
-- /postValCheck/ l- s-
-- /postLoop/     l+ 0

