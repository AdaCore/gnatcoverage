with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_A_SC is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, False) = False);
end;

--# andthen.adb
--  /eval(Stmt|Other)/  l! c!:"B"
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l+ 0
--  /returnValue/   l+ 0
--  /decl/   l+ 0
