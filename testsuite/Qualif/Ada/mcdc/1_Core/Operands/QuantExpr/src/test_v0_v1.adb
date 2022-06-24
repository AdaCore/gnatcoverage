with Sys, Support; use Sys, Support;

procedure Test_V0_V1 is
begin
   Assert (Sys.Check (V0_V1));
end;

--# sys.ads
-- /return/ l+ ## 0
-- /forall/ l! ## dF-:"SA(I).I0", eF-:"((for"
-- /andthen/ l! ## 0
-- /forsome/ l! ## dF-:"S.I0"
