with Doif_FX_FF;

procedure Test_FX_FF is
begin
   Doif_FX_FF;
end;

--# slists-fault.adb

-- /AF_decl/  l+ ## 0
-- /AF_init/  l+ ## 0
-- /AF_while/ l+ ## 0
-- /AF_ren/   l+ ## 0
-- /AF_evA/   l! ## dT-
-- /AF_skip/  l- ## s-
-- /AF_evLB/  l! ## dT-
-- /AF_evHB/  l! ## 0
-- /AF_fault/ l- ## s-
-- /AF_ok/    l+ ## 0
-- /AF_next/  l+ ## 0

