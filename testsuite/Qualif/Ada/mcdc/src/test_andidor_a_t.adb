with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_A_T is
begin
   Assert (F (True , True, True) = True);
   Assert (F (False, True, True) = False);
end;

--# andidor.adb
-- /evaluate/      l! c!:"Id",dF-:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
