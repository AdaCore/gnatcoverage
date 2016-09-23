
with Ops, Support; use Ops, Support;

procedure Test_Ops_Q4 is
begin
   Assert (Quarter (X => -1, Y => 5) = Q4);
end;

--# ops.adb
--  /stmt/     l+ ## 0
--  /test-q1/  l+ ## 0
--  /q1/       l- ## s-
--  /test-q2/  l+ ## 0
--  /q2/       l- ## s-
--  /test-q3/  l+ ## 0
--  /q3/       l- ## s-
--  /test-q4/  l+ ## 0
--  /q4/       l+ ## 0
--  /q0/       l- ## s-
