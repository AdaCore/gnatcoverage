
with Support, Ops; use Ops, Support;

procedure Test_And_TT is
begin
   Assert (Ops.Eval (Op_Andthen, True, True) = True);
end;

--# ops/ops.adb
-- /eval_and/ l+ ## 0
-- /eval_or/  l- ## s-

--# ops/ops-andthen.adb
-- /eval/ l! ## eF-

--# ops/ops-orelse.adb
-- /eval/ l- ## s-
