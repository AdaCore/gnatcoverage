with Pkg;

--  All the contracts are evaluated to True, but always through the
--  short-circuit path, so their second condition is never evaluated.

procedure Test_Short is
   X : Boolean;
begin
   X := Pkg.Foo (True, True);
   X := Pkg.Bar (True, True);

   declare
      Dummy : constant Pkg.Rec := Pkg.Make_Rec (True);
   begin
      null;
   end;
end Test_Short;

--# pkg.ads
-- /foo_pre/   a=>l+, c=>l! ## a=>0, c=>ac!
-- /foo_post/  a=>l+, c=>l! ## a=>0, c=>ac!
-- /rec_decl/  l+ ## 0
-- /rec_def/   l+ ## 0
-- /rec_b/     l+ ## 0
-- /rec_end/   l+ ## 0
-- /type_inv/  l+ ## 0
-- /make_rec/  l+ ## 0

--# pkg.adb
-- /foo_ret/   l+ ## 0
-- /bar_pre/   a=>l+, c=>l! ## a=>0, c=>ac!
-- /bar_ret/   l+ ## 0
