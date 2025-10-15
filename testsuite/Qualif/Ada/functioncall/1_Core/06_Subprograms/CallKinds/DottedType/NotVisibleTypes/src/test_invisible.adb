with Make_Call;

--  LIMITATION : invisible dotted types
--  gnatcov cannot instrument calls to functions returning types which are not
--  visible from the unit the call is made from. We expect to report the
--  coverage of such calls as undetermined.

procedure Test_Invisible is
begin
   Make_Call;
end Test_Invisible;

--# make_call.adb
-- /test/     l+ ## 0
-- /decl/     l+ ## 0
-- /dummy/    l? ## c?

--# pkg1.ads
-- /decl/     l+ ## 0

--# pkg2.ads
-- /fun/      l+ ## 0
