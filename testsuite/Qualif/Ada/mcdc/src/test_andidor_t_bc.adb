with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_BC is
begin
   Assert (F (True, False, False) = False); -- Id B C
   Assert (F (True, False, True)  = True);  -- Id   C
   Assert (F (True, True , False) = True);  -- Id B
end;

--# andidor.adb
-- /evaluate/      l! c!:"A"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
