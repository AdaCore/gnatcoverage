with Support, Levels.NOF; use Support, Levels, Levels.NOF;

--P Exercize N_Of with a non empty sample and a level nowehere to be found

procedure Test_Levels_NOF_N0 is
   S : Sample := (1, 2, 3, 4);
begin
   Assert (N_Of (0, S) = 0);
end;

--# levels-nof.adb
-- /decl/ l+ 0
-- /loopExit/ l+ 0
-- /loopBody/ l+ 0
-- /incN/ l- s-
-- /returnValue/ l+ 0

