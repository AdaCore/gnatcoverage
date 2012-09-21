with Doif_FX_FT, Doif_FX_TX, Doif_FX_FF;
with Doif_TF_FT, Doif_TF_TX, Doif_TF_FF;

procedure Test_DF_FU is
begin
   Doif_FX_FT;   
   Doif_TF_FT;
   
   Doif_FX_TX;
   Doif_TF_TX;
   
   Doif_FX_FF;
   Doif_TF_FF;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l! ## dT-
-- /AF_skip/  l- ## s-
-- /AF_evLB/  l+ ## 0
-- /AF_evHB/  l+ ## 0
-- /AF_fault/ l+ ## 0
-- /AF_ok/    l+ ## 0
-- /AF_next/  l+ ## 0

