with Make_Call;

--  LIMITATION
--  gnatcov is currently unable to determine the coverage of calls made
--  within dotted names.
--
--  Make no call to a subprogram returning a composite type, and attempt to
--  access a field of the return value.
--  Test that the coverage of such a call is reported as undertermined. The
--  called function sould however be marked as not covered as the subprogram
--  is never executed.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# make_call.adb
-- /test/   l- ## f-
-- /decl/   l- ## s-
-- /fun/    l- ## f-
-- /call_0/ l- ## s-,c-
-- /call_1/ l- ## s-,c?
-- /null/   l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l- ## s-,f-
