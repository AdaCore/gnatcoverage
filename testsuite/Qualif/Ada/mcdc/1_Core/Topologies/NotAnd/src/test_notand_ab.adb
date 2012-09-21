with Support, Notand ; use Support, Notand;

procedure Test_Notand_AB is
begin
   Assert (F (False, True) = True);
   Assert (F (False, False) = False);
   Assert (F (True, True) = False);
end;

--# notand.adb
--  /eval(Stmt|Other)/  l+ ## 0
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   l+ ## 0
