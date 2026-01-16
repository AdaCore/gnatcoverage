with Pkg; use Pkg;

procedure Test_Pkg_Full is
begin
   Set_Defaults (-1, 1);
   declare
      P : Point;
   begin
      Print (P);
   end;
end Test_Pkg_Full;

--# pkg.ads
--
-- /dic/ l+ ## 0
