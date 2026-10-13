with Silent_Last_Chance;
with Ada.Assertions;

with Pkg;

--  Build a record that violates its type invariant: the invariant is evaluated
--  but never to True.

procedure Test_Fail_Inv is
begin
   begin
      declare
         Dummy : constant Pkg.Rec := Pkg.Make_Rec (False);
      begin
         null;
      end;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Test_Fail_Inv;

--# pkg.ads
-- /foo_pre/   l- ## a-
-- /foo_post/  l- ## a-
-- /rec_decl/  l+ ## 0
-- /rec_def/   l+ ## 0
-- /rec_b/     l+ ## 0
-- /rec_end/   l+ ## 0
-- /type_inv/  l- ## aT-
-- /make_rec/  l+ ## 0

--# pkg.adb
-- /foo_ret/   l- ## s-
-- /bar_pre/   l- ## a-
-- /bar_ret/   l- ## s-
