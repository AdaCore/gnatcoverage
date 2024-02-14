with Math; use Math;
with Support; use Support;

procedure Test_Add is
begin
   Assert (Eval (Op => Add, X => 4, Y => 5) = 9);
end;

--# math.adb
--  /check-add/  l! ## dF-
--  /add/        l+ ## 0
--  /check-mult/ l- ## s-
--  /mult/       l- ## s-
