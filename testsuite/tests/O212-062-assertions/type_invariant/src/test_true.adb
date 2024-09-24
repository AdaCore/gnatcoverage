with Pkg;

--  Assertion test for type invariant of a record.
--
--  Creating a record with a field value that respects the type invariant.
--  This raises no assertion error. Test that the coverage state of the type
--  invariant is set to "covered".

procedure Test_True
is
   use Pkg;

   Dummy : Rec := Make_Rec (True);
begin
   null;
end Test_True;

--# pkg.ads
-- /rec_decl/ l+ ## 0
-- /rec_def/  l+ ## 0
-- /rec_b/    l+ ## 0
-- /rec_end/  l+ ## 0
-- /type_inv/ l+ ## 0
-- /return/   l+ ## 0
