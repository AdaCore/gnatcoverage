with Ops, Support; use Ops, Support;

procedure Test_Long_Xzero is
begin
   Tryme (A => "A", N => 23, B => "B", X => 0);
   Check (Which => Long, S => "A*23");
end;

--# ops.adb
--  /tryme/ l+ ## 0
--  /test-len/ l! ## dF-
--  /long/ l+ ## 0
--  /short/ l- ## s-
--  /test-xpos/ l! ## dT-
--  /xpos/ l- ## s-
--  /comp-abx/ l- ## s-
--  /abab/ l- ## s-
--  /test-xneg/ l! ## dT-
--  /xneg/ l- ## s-
--  /xzero/ l+ ## 0
--  /check-long/ l+ ## 0
--  /check-short/ l- ## s-
--  /check-mapped/ l- ## s-
--  /do-check/ l+ ## 0
