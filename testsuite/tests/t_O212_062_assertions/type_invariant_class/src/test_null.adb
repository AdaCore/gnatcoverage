--  Assertion test for type invariant of a record.
--
--  Test that an non-executed type invariant is reported as such.

with Pkg;

procedure Test_Null is
begin
   null;
end Test_Null;

--# pkg.ads
-- /non_zero_decl/   l+ ## 0
-- /non_zero_def/    l+ ## 0
-- /type_class_inv/  l! ## a-
-- /type_inv/        l! ## a-
-- /return/          l- ## s-
