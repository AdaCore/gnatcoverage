with Pkg; use Pkg;

procedure Test_Pkg_Full is
   P : Point := (10, 20);
begin
   Test (P);
end Test_Pkg_Full;

--# pkg.ads
--
--  /pre/  l+ ## 0
--  /post/ l+ ## 0
--
--# pkg.adb
--
--  /div_2/ l+ ## 0
--  /stmt/  l+ ## 0
--  /check/ l! ## dT-
--  /raise/ l- ## s-
