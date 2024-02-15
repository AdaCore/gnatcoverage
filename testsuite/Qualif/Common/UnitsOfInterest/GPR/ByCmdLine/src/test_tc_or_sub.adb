
with Support, Boolops, Intops; use Intops, Boolops, Support;

procedure Test_TC_Or_Sub is
begin
   Assert (Boolops.Eval (Op_Orelse, False, True) = True);
   Assert (Intops.Eval (Op_Sub, 4, 3) = 1);
end;

--# ../../boolops/src/boolops.adb
-- /eval_or/   l+ ## 0
-- /eval_and/  l- ## s-

--# ../../boolops/src/boolops-andthen.adb
-- /eval/ l- ## s-

--# ../../boolops/src/boolops-orelse.adb
-- /eval/ l! ## eF-

--# ../../intops/src/intops.adb
-- /eval_add/  l- ## s-
-- /eval_sub/  l+ ## 0

--# ../../intops/src/intops-add.adb
-- /eval/ l- ## s-

--# ../../intops/src/intops-sub.adb
-- /eval/ l+ ## 0
