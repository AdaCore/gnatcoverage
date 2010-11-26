with Support, Andidor; use Support, Andidor;

procedure Test_Andidor_AIC is
begin
   Assert (F (True , False, False) = False);  --   I C
   Assert (F (True , False, True)  = True);   -- A I C
   Assert (F (False, True , False) = False);  -- A
end;

--# andidor.adb
-- /eval(Stmt|Other)/      l! m!:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
