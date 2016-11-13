with Ops, Support; use Ops, Support;

procedure Test_Long_Xpos is
begin
   Tryme (A => "A", N => 21, B => "B", X => 1);
   Check (Which => Long, S => "A*21");
end;

--# ops.adb
--  /tryme/ l+ ## 0
--  /test-len/ l! ## dF-
--  /long/ l+ ## 0
--  /short/ l- ## s-
--  /test-xpos/ l! ## dF-
--  /xpos/ l+ ## 0
--  /comp-abx/ l! ## dT-
--  /abab/ l- ## s-
--  /test-xneg/ l- ## s-
--  /xneg/ l- ## s-
--  /xzero/ l- ## s-
--  /check-long/ l+ ## 0
--  /check-short/ l- ## s-
--  /check-mapped/ l- ## s-
--  /do-check/ l+ ## 0
