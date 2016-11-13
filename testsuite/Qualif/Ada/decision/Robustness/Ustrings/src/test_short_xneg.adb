with Ops, Support; use Ops, Support;

procedure Test_Short_Xneg is
begin
   Tryme (A => "A", N => 1, B => "B", X => -2);
   Check (Which => Long, S => "");
   Check (Which => Short, S => "A*1");
   Check (Which => Mapped, S => "");
end;

--# ops.adb
--  /tryme/ l+ ## 0
--  /test-len/ l! ## dT-
--  /long/ l- ## s-
--  /short/ l+ ## 0
--  /test-xpos/ l! ## dT-
--  /xpos/ l- ## s-
--  /comp-abx/ l- ## s-
--  /abab/ l- ## s-
--  /test-xneg/ l! ## dF-
--  /xneg/ l+ ## 0
--  /xzero/ l- ## s-
--  /check-long/ l+ ## 0
--  /check-short/ l+ ## 0
--  /check-mapped/ l+ ## 0
--  /do-check/ l+ ## 0
