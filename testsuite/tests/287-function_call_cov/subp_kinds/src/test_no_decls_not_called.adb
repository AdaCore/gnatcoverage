with No_Decls_Not_Called;

--  Import subprograms which all have a previous declarations.
--  None of the subprograms in the imported package are called here, so all of
--  them should have function coverage violations.

procedure Test_No_Decls_Not_Called is
begin
    null;
end Test_No_Decls_Not_Called;

--# no_decls_not_called.adb
-- /p/    l- ## f-
-- /decl/ l+ ## 0
-- /stmt/ l- ## s-
-- /fun/  l- ## f-
