with Pkg; use Pkg;

procedure Test_Pkg_Full is
   P : Point := (10, 20);
begin
   Test (P);
end Test_Pkg_Full;

--# pkg.ads
--
--  /gen-pre/  l+ ## 0
--  /gen-post/ l+ ## 0
--
--  /acc-pre/  l+ ## 0
--  /acc-post/ l+ ## 0
--
--# pkg.adb
--
--  /div_2/ l+ ## 0
--  /stmt/  l+ ## 0
--  /check/ l! ## dT-
--  /raise/ l- ## s-
