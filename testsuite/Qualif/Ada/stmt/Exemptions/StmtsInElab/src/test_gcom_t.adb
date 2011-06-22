with Support, Comi_Init; use Support;

-- Reach gcom with auto-init True - call covered => no exemption

procedure Test_Gcom_T is
begin
   Assert (Comi_Init.Initialized = True);
end;

--# gcom.adb
-- /init_body/ l+ 0
-- /init_call/ l# x0
