with Support, Expr; use Support;

procedure Test_T is
begin
   Expr.As_RHS       (A => True, B => True);
   Expr.As_ACTUAL    (A => True, B => True);
   Expr.As_CASE      (A => True, B => True);
   Expr.As_AGGREGATE (A => True, B => True);
   Expr.As_DECLINIT  (A => True, B => True);
   Expr.As_DISCINIT  (A => True, B => True);
   
   Expr.GB := Expr.As_RETURN (A => True, B => True);
   
   Expr.As_BODYPRECOND  (A => True, B => True);
   Expr.As_BODYPOSTCOND (A => True, B => True);   
   Expr.As_SPECPRECOND  (A => True, B => True);
   Expr.As_SPECPOSTCOND (A => True, B => True);   
   
   Expr.As_DEBUG_ASSERT (A => True, B => True);   
end;

--# expr.adb
--  /eval/   l+ ## 0
--  /freestanding-expr/  l. ## 0
--  /true/   l+ ## 0
--  /false/  l- ## s-

--# expr.ads
--  /eval/   l+ ## 0
