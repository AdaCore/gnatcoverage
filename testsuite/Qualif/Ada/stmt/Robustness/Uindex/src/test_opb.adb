pragma Ada_2012;

with Ops, Support; use Ops, Support;

procedure Test_OPB is
   CV : Composite_V;
   CC : Composite_C;
begin
   CV.B := 5;
   Assert (CV(Op_B) = 5);
   
   CC.B := 12;
   Assert (CC(Op_B) = 12);   
end;

--# ops.adb
--  /indexing/ l+ ## 0
--  /op_a/ l- ## s-
--  /op_b/ l+ ## 0
