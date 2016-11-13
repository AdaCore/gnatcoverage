with Ops, Support; use Ops, Support;

procedure Test_Abab_T is
begin
   Tryme (A => "A", N => 1, B => "B", X => 2);
   Check (Which => Long, S => "");
   Check (Which => Short, S => "A*1");
   Check (Which => Mapped, S => "22");
end;

--# ops.adb
--  /tryme/ l+ ## 0
--  /test-len/ l! ## dT-
--  /long/ l- ## s-
--  /short/ l+ ## 0
--  /test-xpos/ l! ## dF-
--  /xpos/ l+ ## 0
--  /comp-abx/ l! ## dF-
--  /abab/ l+ ## 0
--  /test-xneg/ l- ## s-
--  /xneg/ l- ## s-
--  /xzero/ l- ## s-
--  /check-long/ l+ ## 0
--  /check-short/ l+ ## 0
--  /check-mapped/ l+ ## 0
--  /do-check/ l+ ## 0
