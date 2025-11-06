with Make_Calls;

--  Define subprograms with no prior declaration and call none of them.
--  None of the subprograms defined in No_Decls_Subp are called so all of
--  them should have function coverage violations and all call sites should
--  have call coverage violations.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# decls.adb
-- /subp/       l- ## f-
-- /stmt/       l- ## s-

--# make_calls.adb
-- /test_proc/  l- ## f-
-- /decl/       l- ## s-
-- /dummy/      l- ## s-
-- /fcall/      l- ## c-
-- /pcall/      l- ## s-,c-
