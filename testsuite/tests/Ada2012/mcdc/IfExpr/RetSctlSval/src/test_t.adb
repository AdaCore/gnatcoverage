with Expr, Support; use Expr, Support;

procedure Test_T is
begin
   Assert (Filter (5, 8) = True); -- A > 0
end;

--# expr.adb
--  /then-bgt/  l+ ## 0
--  /else-agt/  l+ ## 0
--
--%opts: --trace-mode=bin
--  /ctl-apos/  l! ## d!
--
--%opts: --trace-mode=src
--  /ctl-apos/  l! ## dF-
