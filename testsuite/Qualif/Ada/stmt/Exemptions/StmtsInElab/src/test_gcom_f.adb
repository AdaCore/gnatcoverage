with Support, Comi_Noinit; use Support;

-- Reach gcom with auto-init False - call not covered => exempted.

procedure Test_Gcom_F is
begin
   Assert (Comi_Noinit.Initialized = False);
end;

--# gcom.adb
-- /init_body/ l- ## s-
-- /init_call/ l* ## x+
