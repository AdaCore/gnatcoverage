--  Assertion test for type invariant of a record.
--
--  Test that an non-executed type invariant is reported as such.

procedure Test_Null is
begin
   null;
end Test_Null;

--# pkg.ads
-- /rec_decl/ l- ## s-
-- /rec_def/  l- ## s-
-- /rec_b/    l- ## 0
-- /rec_end/  l- ## 0
-- /type_inv/ l- ## a-
-- /return/   l- ## s-
