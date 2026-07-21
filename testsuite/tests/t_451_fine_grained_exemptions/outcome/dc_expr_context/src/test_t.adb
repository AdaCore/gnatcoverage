with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, True, "some message");
end Test_T;

--# pkg.adb
--  /expr_cond/ l+ ## 0
--  /if_cond/   l! ## dF-
--  /put_line/  l+ ## 0
