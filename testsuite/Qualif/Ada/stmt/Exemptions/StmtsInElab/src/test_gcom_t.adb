with Support, Gcom; use Support;

-- Instanciate gcom with auto-init True - call covered.

procedure Test_Gcom_T is
   package Com_Auto is new Gcom (Auto_Init => True);
begin
   Assert (Com_Auto.Initialized = True);
end;

--# gcom.adb
-- /init_body/ l+ 0
-- /init_call/ l# x0
