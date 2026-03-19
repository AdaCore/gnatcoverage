with Support; use Support;

with Pkg; use Pkg;

procedure Test_Full is
begin
   Assert (F (0) = 1);
end Test_Full;

--# pkg.ads
--
-- /stmt/ l+ ## 0
