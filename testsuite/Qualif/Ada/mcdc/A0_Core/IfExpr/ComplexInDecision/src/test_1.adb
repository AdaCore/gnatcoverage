with Pkg;     use Pkg;
with Support; use Support;

procedure Test_1 is
begin
   Assert (P (True, True, True, True, True, True));
end Test_1;

--# pkg.ads
--
-- /decision-1-if-expr/ l! ## eF-
-- /decision-2-if-expr/ l! ## e-
-- /last-cond/          l! ## 0
--
--%opts: --trace-mode=bin
-- /eval-if-expr/       l! ## eF-, d!
--
--%opts: --trace-mode=src
-- /eval-if-expr/       l! ## eF-, dF-
