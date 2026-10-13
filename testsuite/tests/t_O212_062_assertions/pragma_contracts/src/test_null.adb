with Pkg;

--  Nothing is called: all the contracts must be reported as never evaluated.

procedure Test_Null is
begin
   null;
end Test_Null;

--# pkg.ads
-- /foo_pre/   l- ## a-
-- /foo_post/  l- ## a-
-- /rec_decl/  l+ ## 0
-- /rec_def/   l+ ## 0
-- /rec_b/     l+ ## 0
-- /rec_end/   l+ ## 0
-- /type_inv/  l- ## a-
-- /make_rec/  l- ## s-

--# pkg.adb
-- /foo_ret/   l- ## s-
-- /bar_pre/   l- ## a-
-- /bar_ret/   l- ## s-
