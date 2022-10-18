with Pkg;     use Pkg;
with Support; use Support;

procedure Test_Full is
begin
   Assert (P (True, True, True, True, True, True));
   Assert (P (True, False, True, True, True, True));
   Assert (P (True, True, False, True, True, True));
   Assert (P (False, True, True, True, True, True));
   Assert (P (False, True, True, False, True, True));
   Assert (P (False, True, True, True, False, True));

   Assert (not P (False, True, True, True, False, False));
end Test_Full;

--# pkg.ads
--
-- /eval-if-expr/       l+ ## 0
-- /decision-1-if-expr/ l+ ## 0
-- /decision-2-if-expr/ l+ ## 0
-- /last-cond/          l+ ## 0
