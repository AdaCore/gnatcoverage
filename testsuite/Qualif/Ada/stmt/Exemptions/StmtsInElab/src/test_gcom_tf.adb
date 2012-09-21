with Support, Comi_Init, Comi_Noinit; use Support;

--  Reach gcom with auto-init both False and True - call covered => exempted

procedure Test_Gcom_TF is
begin
   Assert (Comi_Init.Initialized = True);
   Assert (Comi_Noinit.Initialized = False);
end;

--# gcom.adb
-- /init_body/ l+ ## 0
-- /init_call/ l# ## x0
