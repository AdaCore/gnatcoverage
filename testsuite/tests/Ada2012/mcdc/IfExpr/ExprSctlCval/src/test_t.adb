with Expr, Support; use Expr, Support;

procedure Test_T is
begin
   Assert (Eval (Op_And, True, True) = True);
end;

--# expr.ads
--  /then-and/ l! ## eF-
--  /else-or/  l! ## e-
--
--%opts: --trace-mode=bin
--  /if-opand/ l! ## d!
--
--%opts: --trace-mode=src
--  /if-opand/ l! ## dF-
