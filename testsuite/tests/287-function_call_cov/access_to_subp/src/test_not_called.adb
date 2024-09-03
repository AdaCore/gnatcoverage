with Access_To_Subp;

--  Check function and call coverage for an acces to a subprogram.
--  Here, no calls are made so expect function and call violation for all such
--  SCOs.

procedure Test_Not_Called is
begin
    null;
end;

--# access_to_subp.adb
-- /decl/   l- ## s-
-- /dstmt/  l- ## 0
-- /fun/    l- ## f-
-- /call/   l- ## s=>s-, f=>s-,c-

--# pkg.ads
-- /decl/   l+ ## 0
-- /stmt/   l- ## s=><-, f=>s-,f-
