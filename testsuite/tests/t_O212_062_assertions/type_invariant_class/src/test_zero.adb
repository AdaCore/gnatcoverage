with Silent_Last_Chance;
with Ada.Assertions;

with Pkg; use Pkg;

--  Assertion test for type invariant of a record.
--
--  Create a record with a field value that does not respect the type
--  invariant. This raises an assertion error. Test that the coverage state of
--  the type invariant is set to "not covered".

procedure Test_Zero is
   Dummy : constant Sub_Non_Zero := Make_Rec (0);
begin
   null;
exception
   when Ada.Assertions.Assertion_Error =>
      null;
end Test_Zero;

--# pkg.ads
-- /non_zero_decl/   l+ ## 0
-- /non_zero_def/    l+ ## 0
-- /type_class_inv/  l! ## aT-
-- /type_inv/        l! ## a-
-- /return/          l+ ## 0
