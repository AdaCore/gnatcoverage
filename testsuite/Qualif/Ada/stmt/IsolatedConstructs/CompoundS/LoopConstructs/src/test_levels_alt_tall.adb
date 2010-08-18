with Support, Levels.ALT; use Support, Levels, Levels.ALT;

--P Exercize All_LT True from all elements in non-empty sample

procedure Test_Levels_ALT_Tall is
   S : Sample := (2, 3, 2, 3);
begin
   Assert (All_LT (5, S));
end;

--# levels-alt.adb
-- /decl/         l+ 0
-- /preLoop/      l+ 0
-- /indexCheck/   l+ 0
-- /preValCheck/  l+ 0
-- /valueCheck/   l+ 0
-- /postValCheck/ l+ 0
-- /postLoop/     l+ 0

