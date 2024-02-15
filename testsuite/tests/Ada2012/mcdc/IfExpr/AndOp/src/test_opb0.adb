with Expr, Support; use Expr, Support;

procedure Test_Opb0 is
begin
   --   A op1 B op2 Outer
   --   F F   X X   F

   Assert (Filter (A => False, Valat => False, Valaf => False,
                   B => False, Valbt => True, Valbf => True) = False);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"A", d-:"B", eT-:"if A"
--
--%opts: --trace-mode=src
--  /eval/ l! ## dT-:"A", d-:"B", eT-:"if A"
