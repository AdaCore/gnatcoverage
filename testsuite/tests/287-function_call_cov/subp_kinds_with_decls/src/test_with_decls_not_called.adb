with With_Decls;

--  Import subprograms which all have a previous declarations.
--  None of the subprograms in the imported package are called here, so all of
--  them should have function coverage violations.

procedure Test_With_Decls_Not_Called is
begin
    null;
end Test_With_Decls_Not_Called;

--# with_decls.ads
-- /stmt/ l+ ## 0
--# with_decls.adb
-- /stmt/ l- ## s-
-- /fun/  l- ## f-
