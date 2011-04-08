with Support, Ops, Ctl; use Support;

procedure Test_Ops_Inc is
   X : Integer := 12;
begin
   Ops.Inc (X);
   Assert (X = 13 and then Ops.V = 2);
end;

--# ops.adb
-- /incOp/     l+ 0
-- /incCheck/  l. 0
-- /incCount/  l. 0
-- /elabOp/    l+ 0
-- /elabCheck/ l. 0
-- /elabCount/ l. 0
