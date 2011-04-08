with Support, Ops, Ctl; use Support;

procedure Test_Ops_0 is
begin
   Assert (Ops.V = 2);
end;

--# ops.adb
-- /incOp/     l- s-
-- /incCheck/  l. 0
-- /incCount/  l. 0
-- /elabOp/    l+ 0
-- /elabCheck/ l. 0
-- /elabCount/ l. 0
