with Make_Call;

-- LIMITATION
-- gnatcov is currently unable to determine the coverage of calls made
-- within dotted names.
--
-- Make a call to a subprogram returning a composite type and attempt to
-- access a field of the return value.
-- Test that the coverage of such a call is reported as undertermined. The
-- called function sould however be marked as covered as the subprogram is
-- called.

procedure Test_Call is
begin
   Make_Call;
end Test_Call;

--# make_call.adb
-- /test/   l+ ## 0
-- /decl/   l+ ## 0
-- /fun/    l+ ## 0
-- /call_0/ l+ ## 0
-- /call_1/ l? ## c?
-- /null/   l+ ## 0

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l+ ## 0
