with Make_Call;

--  LIMITATION
--  gnatcov is currently unable to determine the coverage of calls made
--  within dotted names when its return type is declared in a unit wich is not
--  imported by the unit where the call is made from.
--
--  Make no call to subprograms returning composite types and attempt to
--  access a field of the return value.
--  Test that the coverage of such a call is reported as undertermined when
--  the return type is not visible from the call's unit. Also test that the
--  the coverage state in "not covered" when the return type is imported.
--  In both cases, the called functions should however be marked as not
--  covered as the subprogram is called.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# make_call.adb
-- /test/   l- ## f-
-- /decl/   l- ## s-
-- /fun/    l- ## f-
-- /call_0/ l- ## s-,c-
-- /call_1/ l- ## s-,c-
-- /call_a/ l- ## s-,c?
-- /call_b/ l- ## s-,c?
-- /call_c/ l- ## s-,c-
-- /call_d/ l- ## s-,c-
-- /call_e/ l- ## s-,c-
-- /call_f/ l- ## s-,c-
-- /null/   l- ## s-

--# pkg.ads
-- /decl/   l+ ## 0
-- /set/    l- ## s-,f-

--# pkg1.ads
-- /decl/   l+ ## 0

--# pkg1-child.ads
-- /decl/   l+ ## 0

--# pkg2.ads
-- /fun/   l- ## s-,f-

--# pkg3.ads
-- /decl/   l+ ## 0

--# pkg3-child.ads
-- /decl/   l+ ## 0

--# pkg4.ads
-- /fun/   l- ## s-,f-

--# pkg5.ads
-- /decl/  l+ ## 0

--# pkg5.adb
-- /fun/   l- ## s-,f-

--# pkg6.ads
-- /fun/  l- ## s-,f-,c-,c-

--# pkg6.adb
-- /fun/  l- ## s-,f-,c-

--# pkg7.ads
-- /decl/  l+ ## 0

--# pkg7-child.ads
-- /fun/   l- ## s-,f-
