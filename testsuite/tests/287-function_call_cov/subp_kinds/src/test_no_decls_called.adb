with No_Decls_Called;

--  Call the subprograms which have a previous declaration.

procedure Test_No_Decls_Called is
begin
    No_Decls_Called;
end Test_No_Decls_Called;

--# no_decls_called.adb
-- /p/          l+ ## 0
-- /decl/       l+ ## 0
-- /fun/        l+ ## 0
-- /stmt/       l+ ## 0
-- /call/       l+ ## 0
-- /set_f/      l+ ## 0
-- /if_cond/    l! ## dT-
-- /v_dummy/    l- ## s-
-- /v_stmt/     l- ## s-,c-
-- /v_call/     l- ## c-
-- /v_scall/    l- ## s=>s-, f=>s-,c-
