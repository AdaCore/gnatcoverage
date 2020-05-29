
with Support, Ops; use Ops, Support;

procedure Test_And_TF is
begin
   Assert (Ops.Eval (Op_Andthen, True, False) = False);
end;

--# ops.adb
-- /eval_and/ l+ ## 0
-- /eval_or/  l- ## s-

--# ops-andthen.adb
-- /eval/ l! ## eT-

--# ops-orelse.adb
-- /eval/ l- ## s-


