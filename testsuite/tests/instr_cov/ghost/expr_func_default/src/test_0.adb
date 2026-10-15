with Pkg;

procedure Test_0 is
   package Inst_Pkg is new Pkg;
begin
   Inst_Pkg.Print (2);
end Test_0;

--# pkg.adb
--  /stmt/              l+ ## 0

--# pkg.ads
--  /precond/           l. ## 0
--
--%opts: !--instrument-ghost
--  /expr_func_default/ l. ## 0
--
--%opts: --instrument-ghost
--  /expr_func_default/ l+ ## 0
