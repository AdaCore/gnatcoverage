with Support, Gcom; use Support;

-- Instanciate gcom with auto-init False - call exempted. Force a variable
--  value to preventtotal absence of code from constant folding otherwise.

procedure Test_Gcom_F is
   package Com_Noinit is new Gcom (Auto_Init => Identity (False));
begin
   Assert (Com_Noinit.Initialized = False);
end;

--# gcom.adb
-- /init_body/ l- s-
-- /init_call/ l* x+
