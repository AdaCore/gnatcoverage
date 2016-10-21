with Support, Andthen ; use Support, Andthen;

procedure Test_Andthen_AB is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (True, False) = False);
   Assert (And_Then (False, True) = False);
end;

--# andthen.ads andthen.adb
--  /eval(Stmt|Other)/  l+ ## 0
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
