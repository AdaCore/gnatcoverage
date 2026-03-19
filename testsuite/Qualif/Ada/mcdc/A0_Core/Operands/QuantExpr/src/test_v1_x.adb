with Sys, Support; use Sys, Support;

procedure Test_V1_X is
begin
   Assert (not Sys.Check (V1_X));
end;

--# sys.ads
-- /return/ l+ ## 0
-- /forall/ l! ## dT-:"SA(I).I0", eT-:"((for"
-- /andthen/ l! ## 0
-- /forsome/ l! ## d-:"S.I0"
