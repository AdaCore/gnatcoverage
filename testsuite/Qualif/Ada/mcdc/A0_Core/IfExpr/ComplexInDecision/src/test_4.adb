with Pkg;     use Pkg;
with Support; use Support;

procedure Test_4 is
begin
   Assert (P (False, True, True, True, False, True));
   Assert (P (True, True, True, True, True, False));
   Assert (not P (False, True, True, False, True, False));
end Test_4;

--# pkg.ads
--
-- /eval-if-expr/       l+ ## 0
-- /decision-1-if-expr/ l! ## eF-
-- /decision-2-if-expr/ l! ## eT-
-- /last-cond/          l+ ## 0
