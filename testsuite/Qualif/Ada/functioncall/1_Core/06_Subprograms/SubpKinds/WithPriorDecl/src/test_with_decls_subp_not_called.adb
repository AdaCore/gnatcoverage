with Make_Calls;

--  Define subprograms with no prior declaration and call none of them.
--  None of the subprograms defined in No_Decls_Subp are called so all of
--  them should have function coverage violations and all call sites should
--  have call coverage violations.

procedure Test_With_Decls_Subp_Not_Called is
begin
   null;
end Test_With_Decls_Subp_Not_Called;

--# with_decls_subp.adb
-- /subp/       l- ## f-
-- /stmt/       l- ## s-

--# make_calls.adb
-- /test_proc/  l- ## f-
-- /decl/       l- ## s-
-- /dummy/      l- ## s-
-- /fcall/      l- ## c-
-- /pcall/      l- ## s-,c-
