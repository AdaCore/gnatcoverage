with No_Decls_Subp;

--  Define subprograms with no prior declaration and call none of them.
--  None of the subprograms defined in No_Decls_Subp are called so all of
--  them should have function coverage violations and all call sites should
--  have call coverage violations.

procedure Test_No_Decls_Subp_Not_Called is
begin
    No_Decls_Subp (Make_Calls => False);
end Test_No_Decls_Subp_Not_Called;

--# no_decls_subp.adb
-- /test_proc/  l+ ## 0
-- /decl/       l+ ## 0
-- /subp/       l- ## f-
-- /stmt/       l- ## s-
-- /if_cond/    l! ## dT-
-- /dummy/      l- ## s-
-- /fcall/      l- ## c-
-- /pcall/      l- ## s-,c-
