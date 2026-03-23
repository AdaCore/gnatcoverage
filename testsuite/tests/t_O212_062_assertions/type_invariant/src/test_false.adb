with Silent_Last_Chance;
with Ada.Assertions;

with Pkg;

--  Assertion test for type invariant of a record.
--
--  Create a record with a field value that does not respect the type
--  invariant. This raises an assertion error. Test that the coverage state of
--  the type invariant is set to "not covered".

procedure Test_False
is
   use Pkg;
begin
   begin
      declare
         Dummy : constant Rec := Make_Rec (False);
      begin
         null;
      end;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Test_False;

--# pkg.ads
-- /rec_decl/ l+ ## 0
-- /rec_def/  l+ ## 0
-- /rec_b/    l+ ## 0
-- /rec_end/  l+ ## 0
-- /type_inv/ l! ## aT-
-- /return/   l+ ## 0
