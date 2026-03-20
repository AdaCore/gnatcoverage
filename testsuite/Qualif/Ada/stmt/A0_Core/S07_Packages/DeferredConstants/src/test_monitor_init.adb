With Monitor, Environ, Support; use Monitor, Environ, Support;

procedure Test_Monitor_Init is
begin
   Assert (Did_Initial_Probe = True);
   Assert (N_Matches = 0);
end;

--# monitor.ads
--  /decl/  l+ ## 0

--# monitor.adb
--  /compare/  l- ## s-
--  /match/    l- ## s-

--# environ.adb
--  /compare/  l- ## s-
--  /probe/ l+ ## 0
--  /init_probe/ l+ ## 0
--  /other_probe/ l- ## s-
