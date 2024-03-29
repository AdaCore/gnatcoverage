With Monitor, Environ, Support; use Monitor, Environ, Support;

procedure Test_Monitor_1Cmp is
   Current_State : State;
begin
   Assert (Did_Initial_Probe = True);
   Assert (Did_Other_Probe = False);

   Compare_With_Initial_State_Now;
   Assert (Did_Other_Probe = True);
   Assert (N_Matches = 0);
end;

--# monitor.ads
--  /decl/  l+ ## 0

--# monitor.adb
--  /compare/  l+ ## 0
--  /match/    l- ## s-

--# environ.adb
--  /compare/  l+ ## 0
--  /probe/ l+ ## 0
--  /init_probe/ l+ ## 0
--  /other_probe/ l+ ## 0
