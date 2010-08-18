with Support, Levels.NOF; use Support, Levels, Levels.NOF;

--P Exercize multiple calls to N_Of

procedure Test_Levels_NOF_Mult is
   S1 : Sample := (3, 2, 3, 1);
   S2 : Sample (1 .. 0);
begin
   Assert (N_Of (3, S2) = 0);
   Assert (N_Of (3, S1) = 2);
   Assert (N_Of (5, S1) = 0);
end;

--# levels-nof.adb
-- /decl/     l+ 0
-- /loopExit/ l+ 0
-- /loopBody/ l+ 0
-- /incN/     l+ 0
-- /retN/     l+ 0

