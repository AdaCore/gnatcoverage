with Support, Misc, State; use Support, Misc;

procedure Test_Misc is
begin
   Check_State;
   Assert (State.Ticks = 1);
   Assert (State.Dticks = 0);
end;

--# misc.adb
--  /stmt/ l+ ## 0
--  /out/  l0 ## s0

-- %cargs: !-O1
--  /opt_out/ l- ## s-

-- %cargs: -O1
--  /opt_out/ l0 ## s0
