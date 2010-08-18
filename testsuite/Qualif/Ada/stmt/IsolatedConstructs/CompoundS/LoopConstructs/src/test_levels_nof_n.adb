with Support, Levels.NOF; use Support, Levels, Levels.NOF;

--P Exercize N_Of with a non empty sequence and a level to be
--P found in a number of places.

procedure Test_Levels_NOF_N is
   S : Sample := (3, 2, 3, 1);
begin
   Assert (N_Of (3, S) = 2);
end;

--# levels-nof.adb
-- /decl/     l+ 0
-- /loopExit/ l+ 0
-- /loopBody/ l+ 0
-- /incN/     l+ 0
-- /retN/     l+ 0

