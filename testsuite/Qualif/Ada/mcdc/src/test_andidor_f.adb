with Support, Andidor; use Support, Andidor;

procedure Test_AndIdOr_F is
begin
   Assert (F (False, False, False) = False);
   Assert (F (False, False, False) = False);
   Assert (F (False, False, True)  = False);
   Assert (F (False, True , False) = False);
end;

--# andidor.adb
-- /evaluate/      l! dT-:"A"
-- /decisionTrue/  l- s-
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
