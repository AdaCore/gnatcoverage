with Math; use Math;
with Support; use Support;

procedure Test_Mult is
begin
   Assert (Eval (Op => Mult, X => 4, Y => 5) = 20);
end;
