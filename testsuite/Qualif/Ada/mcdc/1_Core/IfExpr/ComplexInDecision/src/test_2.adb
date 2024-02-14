with Pkg;     use Pkg;
with Support; use Support;

procedure Test_2 is
begin
   Assert (P (True, False, True, True, True, True));
end Test_2;

--# pkg.ads
--
-- /decision-1-if-expr/ l! ## eT-
-- /decision-2-if-expr/ l! ## e-
-- /last-cond/          l! ## 0
--
--%opts: --trace-mode=bin
-- /eval-if-expr/       l! ## eF-, d!
--
--%opts: --trace-mode=src
-- /eval-if-expr/       l! ## eF-, dF-
