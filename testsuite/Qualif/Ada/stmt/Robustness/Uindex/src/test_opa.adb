pragma Ada_2012;

with Ops, Support; use Ops, Support;

procedure Test_OPA is
   CV : Composite_V;
   CC : Composite_C := (1, 2);
begin
   CV.A := 5;
   Assert (CV(Op_A) = 5);
   Assert (CC(Op_A) = 1);
end;

--# ops.adb
--  /indexing/ l+ ## 0
--  /op_a/ l+ ## 0
--  /op_b/ l- ## s-
