
with Ops, Support; use Ops, Support;

procedure Test_Ops_Q13 is
begin
   Assert (Quarter (X => 1, Y => 1) = Q1);
   Assert (Quarter (X => -1, Y => -1) = Q3);
end;

--# ops.adb
--  /stmt/     l+ ## 0
--  /test-q1/  l+ ## 0
--  /q1/       l+ ## 0
--  /test-q2/  l+ ## 0
--  /q2/       l- ## s-
--  /test-q3/  l+ ## 0
--  /q3/       l+ ## 0
--  /test-q4/  l- ## s-
--  /q4/       l- ## s-
--  /q0/       l- ## s-
