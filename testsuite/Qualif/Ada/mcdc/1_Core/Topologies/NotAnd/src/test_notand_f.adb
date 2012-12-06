with Support, Notand; use Support, Notand;

procedure Test_Notand_F is
begin
   Assert (F (True, False) = False);
   Assert (F (True, True) = False);
   Assert (F (False, False) = False);
end;

--# notand.adb
--  /eval(Stmt|Other)/  l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

