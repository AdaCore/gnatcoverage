with Make_Calls;

--  Define subprograms with no prior declaration and call all of them.
--  Check that there are no function coverage violations for the subprograms
--  and no call coverage violation for call sites.

procedure Test_Call is
begin
   Make_Calls;
end Test_Call;

--# decls.adb
-- /subp/       l+ ## 0
-- /stmt/       l+ ## 0

--# make_calls.adb
-- /test_proc/  l+ ## 0
-- /decl/       l+ ## 0
-- /dummy/      l+ ## 0
-- /fcall/      l+ ## 0
-- /pcall/      l+ ## 0
