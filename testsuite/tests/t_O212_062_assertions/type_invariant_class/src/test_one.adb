with Pkg; use Pkg;

--  Assertion test for type invariant of a record.
--
--  Creating a record with a field value that respects the type invariant.
--  This raises no assertion error. Test that the coverage state of the type
--  invariant is set to "covered".

procedure Test_One
is
   Dummy : Sub_Non_Zero := Make_Rec (1);
begin
   null;
end Test_One;

--# pkg.ads
-- /non_zero_decl/   l+ ## 0
-- /non_zero_def/    l+ ## 0
-- /type_class_inv/  l+ ## 0
-- /type_inv/        l! ## a-
-- /return/          l+ ## 0
