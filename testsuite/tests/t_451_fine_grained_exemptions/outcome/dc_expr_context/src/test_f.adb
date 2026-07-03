with Pkg;

procedure Test_F is
begin
   Pkg.Print_If (False, False, "some message");
end Test_F;

--# pkg.adb
--  /expr_cond/ l+ ## 0
--  /if_cond/   l! ## dT-
--  /put_line/  l- ## s-
