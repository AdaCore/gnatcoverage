with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, False) = False);
   Assert (And_Then (False, True) = False);
   Assert (And_Then (True, False) = False);
end;

--# andthen.ads andthen.adb
--  /eval(Stmt|Other)/  l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

