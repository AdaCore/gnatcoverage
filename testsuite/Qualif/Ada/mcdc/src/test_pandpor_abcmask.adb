with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_ABCmask is
begin
   Assert (F (False, False, True)  = True);
   Assert (F (False, True , False) = False);
   Assert (F (True , False, False) = False);
   Assert (F (True , True , False) = True);
end;

--# pandpor.adb
-- /evaluate/      l+ 0
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0

