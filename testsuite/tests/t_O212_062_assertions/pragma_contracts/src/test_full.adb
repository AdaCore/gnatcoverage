with Pkg;

--  Every condition of every contract is evaluated during an evaluation of its
--  decision to True.

procedure Test_Full is
   X : Boolean;
begin
   X := Pkg.Foo (True, True);
   X := Pkg.Foo (False, True);
   X := Pkg.Bar (True, True);
   X := Pkg.Bar (False, False);

   declare
      Dummy : constant Pkg.Rec := Pkg.Make_Rec (True);
   begin
      null;
   end;
end Test_Full;

--# pkg.ads
-- /foo_pre/   l+ ## 0
-- /foo_post/  l+ ## 0
-- /rec_decl/  l+ ## 0
-- /rec_def/   l+ ## 0
-- /rec_b/     l+ ## 0
-- /rec_end/   l+ ## 0
-- /type_inv/  l+ ## 0
-- /make_rec/  l+ ## 0

--# pkg.adb
-- /foo_ret/   l+ ## 0
-- /bar_pre/   l+ ## 0
-- /bar_ret/   l+ ## 0
