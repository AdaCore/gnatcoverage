with Support, AndPorP; use Support, AndPorP;

--  everything is not covered
procedure Test_AndPorP_0 is
begin
   Assert (True);
end;

--# andporp.adb andporp.ads
--  /evalStmt/      l- ## s-
--  /evalOther/     l- ## 0c
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l- ## s-
--  /returnValue/   l- ## s-
--  /decl/         ~l- ## ~s-
