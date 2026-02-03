with Pkg;

procedure Test_Pkg_0 is
begin
   null;
end Test_Pkg_0;

--# pkg.ads
--
--  /gen-pre/  l- ## a-
--  /gen-post/ l- ## a-
--
--  /acc-pre/  l! ## a-
--  /acc-post/ l! ## a-
--
--# pkg.adb
--
--  /div_2/ l- ## s-
--  /stmt/  l- ## s-
--  /check/ l- ## s-
--  /raise/ l- ## s-
