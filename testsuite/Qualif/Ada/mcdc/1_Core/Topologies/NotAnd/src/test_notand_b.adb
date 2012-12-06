with Support, Notand; use Support, Notand;

procedure Test_Notand_B is
begin
   Assert (F (False, True) = True);
   Assert (F (False, False) = False);
end;

--# notand.adb
--  /eval(Stmt|Other)/  l! ## c!:"A"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0
