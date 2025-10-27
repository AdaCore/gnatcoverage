with Make_Call;

-- LIMITATION
-- gnatcov is currently unable to determine the coverage of calls made
-- within dotted names.
--
-- LIMITATION
-- gnatcov is currently unable to instrument and report coverage for
-- primitives of tagged types.
--
-- Make calls to a subprogram returning a tagged record and attempt to
-- access a field of the return value.
-- Test that the coverage of such a call is reported as undertermined. The
-- called function sould however be marked as covered as the subprogram is
-- called.

procedure Test_Primitive_Calls is
begin
   Make_Call;
end Test_Primitive_Calls;

--#make_call.adb
-- /decl/     l+ ## 0
-- /A1/       l? ## c?
-- /A2/       l+ ## 0
-- /B1/       l? ## c?
-- /B2/       l+ ## 0
-- /C1/       l+ ## 0
-- /C2/       l+ ## 0
-- /D1/       l+ ## 0
-- /D2/       l? ## c?
-- /D3/       l+ ## 0
-- /E1/       l? ## c?,c?
-- /E2/       l? ## 0
-- /E3/       l? ## 0
-- /E4/       l+ ## 0
-- /F1/       l? ## c?
-- /F2/       l? ## 0
-- /F3/       l+ ## 0
-- /stmt/     l+ ## 0

--#pkg.ads
-- /decl/     l+ ## 0
-- /dstmt/    l+ ## 0
-- /expr_pri/ l? ## s?,f?
-- /expr_fun/ l+ ## 0

--#pkg.adb
-- /expr/     l+ ## 0
-- /fun/      l+ ## 0
-- /call/     l+ ## 0
