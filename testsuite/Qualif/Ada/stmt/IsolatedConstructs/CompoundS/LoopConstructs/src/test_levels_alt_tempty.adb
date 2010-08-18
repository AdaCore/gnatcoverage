with Support, Levels.ALT; use Support, Levels, Levels.ALT;

--P Exercize All_LT True from empty sample

procedure Test_Levels_ALT_Tempty is
   S : Sample (1 .. 0);
begin
   Assert (All_LT (5, S));
end;

--# levels-alt.adb
-- /decl/         l+ 0
-- /preLoop/      l+ 0
-- /indexCheck/   l+ 0
-- /preValCheck/  l- s-
-- /valueCheck/   l- s-
-- /postValCheck/ l- s-
-- /postLoop/     l+ 0

