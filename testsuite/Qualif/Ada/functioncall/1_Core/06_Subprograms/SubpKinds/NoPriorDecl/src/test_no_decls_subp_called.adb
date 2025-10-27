with No_Decls_Subp;

--  Define subprograms with no prior declaration and call all of them.
--  Check that there are no function coverage violations for the subprograms
--  and no call coverage violation for call sites.

procedure Test_No_Decls_Subp_Called is
begin
    No_Decls_Subp (Make_Calls => True);
end Test_No_Decls_Subp_Called;

--# no_decls_subp.adb
-- /test_proc/  l+ ## 0
-- /decl/       l+ ## 0
-- /subp/       l+ ## 0
-- /stmt/       l+ ## 0
-- /if_cond/    l! ## dF-
-- /dummy/      l+ ## 0
-- /fcall/      l+ ## 0
-- /pcall/      l+ ## 0
