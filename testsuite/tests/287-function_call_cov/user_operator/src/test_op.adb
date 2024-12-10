with Op;

procedure Test_Op is
begin
    Op;
end Test_Op;

--# op.adb
-- /decl/  l+ ## 0
-- /fun/   l+ ## 0
-- /ok/    l+ ## 0
-- /ko/    l- ## s=>s-,f=>s-,c-
-- /A_ko/  l- ## s=>s-,f=>s-,c-,c-,c-
-- /B_ko/  l- ## s=>s-,f=>s-,c-,c-
-- /C_ko/  l- ## s=>s-,f=>s-,c-,c-
-- /D_ko/  l- ## s=>s-,f=>s-,c-,c-
-- /E_ko/  l- ## s=>s-,f=>s-,c-,c-
-- /F_ko/  l- ## s=>s-,f=>s-,c-,c-,c-
-- /if/    l! ## dT-
--# reversed.ads
-- /ok/    l+ ## 0
