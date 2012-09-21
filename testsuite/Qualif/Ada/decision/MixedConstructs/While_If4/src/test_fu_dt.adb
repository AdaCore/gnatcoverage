with Doif_FX_FT, Doif_FX_TX, Doif_TF_FT, Doif_TF_TX, Doif_TT_XX;

procedure Test_FU_DT is
begin
   Doif_FX_FT;
   Doif_FX_TX;
   
   Doif_TF_FT;
   Doif_TF_TX;
   
   Doif_TT_XX;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l+ ## 0
-- /AF_skip/  l+ ## 0
-- /AF_evLB/  l! ## dF-
-- /AF_evHB/  l! ## 0
-- /AF_fault/ l+ ## 0
-- /AF_ok/    l- ## s-
-- /AF_next/  l+ ## 0

