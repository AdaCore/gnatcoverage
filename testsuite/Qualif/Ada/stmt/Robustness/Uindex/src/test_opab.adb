pragma Ada_2012;

with Ops, Support; use Ops, Support;

procedure Test_OPAB is
   CV : Composite_V;
   CC : Composite_C;
begin
   CV(Op_A) := 12;
   Assert (CV.A = 12);
   
   CV.B := 5;
   Assert (CV(Op_B) = 5);
   
   CC.A := 12;
   Assert (CC(Op_A) = 12);
   
   CC.B := 5;
   Assert (CC(Op_B) = 5);
end;

--# ops.adb
--  /indexing/ l+ ## 0
--  /op_a/ l+ ## 0
--  /op_b/ l+ ## 0
