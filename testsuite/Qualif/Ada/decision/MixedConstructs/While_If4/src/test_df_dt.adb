with Doif_FX_FT, Doif_FX_TX, Doif_TF_FT, Doif_TF_TX;

-- Sequences that leave decision 1 evaluated False only, and decision 2
-- evaluated True only.

procedure Test_DF_DT is
begin
   Doif_FX_FT;
   Doif_FX_TX;
   
   Doif_TF_FT;
   Doif_TF_TX;
end;

--# slists-fault.adb

-- /AF_decl/  l+ 0
-- /AF_init/  l+ 0
-- /AF_while/ l+ 0
-- /AF_ren/   l+ 0
-- /AF_evA/   l! dT-
-- /AF_skip/  l- s-
-- /AF_evLB/  l! dF-
-- /AF_evHB/  l! 0
-- /AF_fault/ l+ 0
-- /AF_ok/    l- s-
-- /AF_next/  l+ 0

