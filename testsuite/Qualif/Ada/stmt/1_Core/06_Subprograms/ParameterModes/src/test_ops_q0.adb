
with Ops, Support; use Ops, Support;

procedure Test_Ops_Q0 is
begin
   Assert (Quarter (X => 0, Y => -2) = Q0);
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
--  /q4/       l- ## s-
--  /q0/       l+ ## 0
