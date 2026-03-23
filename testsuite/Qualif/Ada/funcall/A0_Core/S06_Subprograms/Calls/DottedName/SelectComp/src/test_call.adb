with Make_Call;

--  LIMITATION
--  gnatcov is currently unable to determine the coverage of calls made
--  within dotted names when its return type is declared in a unit wich is not
--  imported by the unit where the call is made from.
--
--  Make a calls to subprograms returning composite types and attempt to
--  access a field of the return value.
--  Test that the coverage of such a call is reported as undertermined when
--  the return type is not visible from the call's unit. Also test that the
--  correct coverage state is reported when the return type is imported.
--  In both cases, the called functions should however be marked as covered as
--  the subprogram is called.

procedure Test_Call is
begin
   Make_Call;
end Test_Call;

--# make_call.adb
-- /test/   l+ ## 0
-- /decl/   l+ ## 0
-- /fun/    l+ ## 0
-- /call_0/ l+ ## 0
-- /call_1/ l+ ## 0
-- /call_a/ l? ## c?
-- /call_b/ l? ## c?
-- /call_c/ l+ ## 0
-- /call_d/ l+ ## 0
-- /call_f/ l+ ## 0
-- /null/   l+ ## 0

--# pkg.ads
-- /decl/  l+ ## 0
-- /set/   l+ ## 0

--# pkg1.ads
-- /decl/  l+ ## 0

--# pkg1-child.ads
-- /decl/  l+ ## 0

--# pkg2.ads
-- /fun/   l+ ## 0

--# pkg3.ads
-- /decl/  l+ ## 0

--# pkg3-child.ads
-- /decl/  l+ ## 0

--# pkg4.ads
-- /fun/   l+ ## 0

--# pkg5.ads
-- /decl/  l+ ## 0

--# pkg5.adb
-- /fun/   l+ ## 0

--# pkg6.ads
-- /fun/  l+ ## 0

--# pkg6.adb
-- /fun/  l+ ## 0

--# pkg7.ads
-- /decl/  l+ ## 0

--# pkg7-child.ads
-- /fun/   l+ ## 0
