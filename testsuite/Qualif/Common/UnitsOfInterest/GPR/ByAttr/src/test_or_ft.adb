
with Support, Ops; use Ops, Support;

procedure Test_Or_FT is
begin
   Assert (Ops.Eval (Op_Orelse, False, True) = True);
end;

--# ops.adb
-- /eval_and/ l- ## s-
-- /eval_or/  l+ ## 0

--# ops-andthen.adb
-- /eval/ l- ## s-

--# ops-orelse.adb
-- /eval/ l! ## eF-
