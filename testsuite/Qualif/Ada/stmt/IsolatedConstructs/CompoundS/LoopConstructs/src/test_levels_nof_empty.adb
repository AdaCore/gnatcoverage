with Support, Levels.NOF; use Support, Levels, Levels.NOF;

--P Exercize N_Of with empty sample

procedure Test_Levels_NOF_Empty is
   S : Sample (1 .. 0);
begin
   Assert (N_Of (3, S) = 0);
end;

--# levels-nof.adb
-- /decl/     l+ 0
-- /loopExit/ l+ 0
-- /loopBody/ l- s-
-- /incN/     l- s-
-- /retN/     l+ 0

