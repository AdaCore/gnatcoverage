with Call_With_Decls;

--  Call the subprograms which have a previous declaration.

procedure Test_With_Decls_Called is
begin
    Call_With_Decls;
end Test_With_Decls_Called;

--# call_with_decls.adb
-- /stmt/       l+ ## 0
-- /fun/        l+ ## 0
-- /set_f/      l+ ## 0
-- /if_cond/    l! ## dT-
-- /v_stmt/     l- ## s-
-- /v_fun/      l- ## c-
-- /v_sfun/     l- ## s-,c-
-- /v_cstmt/    l- ## s-,c-
--# with_decls.ads
-- /stmt/       l+ ## 0
--# with_decls.adb
-- /fun/        l+ ## 0
-- /stmt/       l+ ## 0
