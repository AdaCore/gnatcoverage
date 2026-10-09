with Pkg;

procedure Test_Full is
   package P is new Pkg.Gen;
begin
   P.Check (0);
end Test_Full;

--# pkg.ads
--
-- /expr/  l+ ## 0
