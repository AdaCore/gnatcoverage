with Make_Call;

procedure Test_Dotted_Type_Not_Imported is
begin
   Make_Call;
end Test_Dotted_Type_Not_Imported;

--#make_call.adb
-- /decl/     l+ ## 0
-- /A1/       l! ## c?
-- /A2/       l+ ## 0
-- /B1/       l! ## c?
-- /B2/       l+ ## 0
-- /C1/       l+ ## 0
-- /C2/       l+ ## 0
-- /D1/       l+ ## 0
-- /D2/       l! ## c?
-- /D3/       l+ ## 0
-- /E1/       l! ## c?
-- /E2/       l! ## 0
-- /E3/       l! ## c?
-- /E4/       l+ ## 0
-- /F1/       l! ## c?
-- /F2/       l! ## c?
-- /F3/       l+ ## 0
-- /stmt/     l+ ## 0
--#pkg.ads
-- /expr_pri/ l- ## s?,f-
-- /decl/     l+ ## 0
-- /dstmt/    l+ ## 0
-- /expr_fun/ l+ ## 0
--#pkg.adb
-- /fun/      l+ ## 0
-- /call/     l+ ## 0
