with Support, Notand; use Support, Notand;

procedure Test_Notand_T is
begin
   Assert (F (False, True) = True);
end;

--# notand.adb
--  /eval(Stmt|Other)/ l! dF-
--  /decisionTrue/  l+ 0
--  /decisionFalse/ l- s-
--  /returnValue/   l+ 0
--  /decl/   l+ 0
