with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_IC is
begin
   Assert (F (True, False, True) = True);
   Assert (F (True, False, False) = False);
end;

--# andidor.adb
-- /evaluate/      l! c!:"A",c!:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
