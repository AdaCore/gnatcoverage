with Support, Andidor; use Support, Andidor;

procedure Test_AndIdOr_AIBC is
begin
   Assert (F (True , False, False) = False);  --   I B C
   Assert (F (True , False, True)  = True);   -- A I   C
   Assert (F (True , True , False) = True);   -- A   B
   Assert (F (False, True , False) = False);  -- A
end;

--# andidor.adb
-- /eval(Stmt|Other)/ l+ 0
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0
-- /decl/   l+ 0
