with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_T_T is
begin
   Assert (F (True, True, True)  = True);
   Assert (F (True, False, True) = True);
   Assert (F (True, True, False) = True);
end;

--# andidor.adb
-- /evaluate/      l! dF-:"A",dF-:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l- s-
-- /returnValue/   l+ 0
