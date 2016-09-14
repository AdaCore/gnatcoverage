with Support; use Support;
with Optypes, Ops; use Optypes, Ops;

procedure Test_Ops_Add is
   A, B : T_Operand := (Valid => True, Value => 2);
begin
   Apply (Op_Add, A, B);
   Assert (A.Value = 4);
end;

--# ops.adb
--  /test_add/ l! ## dF-
--  /add/ l+ ## 0
--  /test_sub/ l- ## s-
--  /sub/ l- ## s-

--# ops.ads
