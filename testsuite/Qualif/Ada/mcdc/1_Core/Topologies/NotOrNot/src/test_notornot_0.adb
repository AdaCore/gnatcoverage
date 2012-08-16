with Support, Notornot; use Support;

procedure Test_Notornot_0 is
begin
   Assert (True);
end;

--# notornot.adb
--  /evalStmt/      l- s-
--  /evalOther/     l- 0c
--  /decisionTrue/  l- s-
--  /decisionFalse/ l- s-
--  /returnValue/   l- s-
--  /typedecl/ ~l- ~s-
--  /decl/   l- s-
