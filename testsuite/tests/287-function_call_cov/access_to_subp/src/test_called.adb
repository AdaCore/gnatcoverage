with Access_To_Subp;

--  Check function and call coverage for an acces to a subprogram.
--  Here, all calls and functions are executed.

procedure Test_Called is
begin
    Access_To_Subp;
end;

--# access_to_subp.adb
-- /decl/   l+ ## 0
-- /dstmt/  l+ ## 0
-- /fun/    l+ ## 0
-- /call/   l+ ## 0

--# pkg.ads
-- /decl/   l+ ## 0
-- /dstmt/  l+ ## 0
-- /stmt/   l+ ## 0
