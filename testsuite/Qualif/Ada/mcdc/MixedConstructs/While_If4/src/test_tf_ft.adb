with Doif_TF_FT;

procedure Test_TF_FT is
begin
   Doif_TF_FT;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l! ## dT-
-- /AF_skip/  l- ## s-
-- /AF_evLB/  l! ## dF-
-- /AF_evHB/  l! ## 0
-- /AF_fault/ l+ ## 0
-- /AF_ok/    l- ## s-
-- /AF_next/  l+ ## 0

