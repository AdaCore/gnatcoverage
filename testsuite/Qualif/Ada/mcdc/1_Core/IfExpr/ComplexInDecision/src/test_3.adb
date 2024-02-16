with Pkg;     use Pkg;
with Support; use Support;

procedure Test_3 is
begin
   Assert (P (True, True, True, True, True, True));
   Assert (P (True, True, False, True, True, True));
end Test_3;

--# pkg.ads
--
-- /decision-1-if-expr/ l! ## c!
-- /decision-2-if-expr/ l! ## e-
-- /last-cond/          l! ## 0
--
--%opts: --trace-mode=bin
-- /eval-if-expr/       l! ## eF-, d!
--
--%opts: --trace-mode=src
-- /eval-if-expr/       l! ## eF-, dF-
