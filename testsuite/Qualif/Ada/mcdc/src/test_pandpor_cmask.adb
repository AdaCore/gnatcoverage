with Support, PandPor; use Support, PandPor;

procedure Test_PandPor_Cmask is
begin
   Assert (F (False, False, True) = True);
   Assert (F (False, True, False) = False);
end;

--# pandpor.adb
-- /evaluate/      l! c!:"A",c!:"B"
-- /decisionTrue/  l+ 0
-- /decisionFalse/ l+ 0
-- /returnValue/   l+ 0

