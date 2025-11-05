with Call;

--  Check the coverage of calls to subprograms in packages and nested
--  packages, which requires the calls to be made using dotted names.

procedure Test_Dotted is
begin
    Call;
end Test_Dotted;

--# call.adb
-- /decl/        l+ ## 0
-- /if_cond/     sdf=>l! ## s=>0, d=>dT-, f=>dT-
-- /v_call_stmt/ l- ## s=>s-, f=>s-,c-
-- /v_call_expr/ sf=>l- ## s=>s-, f=>s-,c-
--# pkg.adb
-- /v_fun/       l- ## f-
-- /v_stmt/      l- ## s-
--# pkg.ads
-- /stmt/        l+ ## 0
