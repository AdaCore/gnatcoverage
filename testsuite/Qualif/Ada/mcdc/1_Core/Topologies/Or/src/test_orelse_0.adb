with Support, Orelse; use Support;

procedure Test_OrElse_0 is
begin
   Assert (True);
end;

--# orelse.adb
--  /evalStmt/      l- ## s-
--  /evalOther/     l- ## 0c
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l- ## s-
--  /returnValue/   l- ## s-
--  /decl/   l- ## s-
