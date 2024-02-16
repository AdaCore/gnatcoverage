
with Support, Ops; use Ops, Support;

procedure Test_Or_FT is
begin
   Assert (Ops.Eval (Op_Orelse, False, True) = True);
end;

--# ops/ops.adb
-- /eval_and/ l- ## s-
-- /eval_or/  l+ ## 0

--# ops/ops-andthen.adb
-- /eval/ l- ## s-

--# ops/ops-orelse.adb
-- /eval/ l! ## eF-
