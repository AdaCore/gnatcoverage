with Support; use Support;
with Optypes, Ops; use Optypes, Ops;

procedure Test_Ops_Sub is
   A, B : T_Operand := (Valid => True, Value => 2);
begin
   Apply (Op_Sub, A, B);
   Assert (A.Value = 0);
end;

--# ops.adb
--  /test_add/ l! ## dT-
--  /add/ l- ## s-
--  /test_sub/ l! ## dF-
--  /sub/ l+ ## 0

--# ops.ads
