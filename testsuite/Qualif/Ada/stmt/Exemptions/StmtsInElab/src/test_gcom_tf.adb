with Support, Gcom; use Support;

--  Instanciate gcom with auto-init both False and True - call covered at the
--  generic source level. Force variable values to prevent total absence of
--  code from constant folding otherwise.

procedure Test_Gcom_TF is
   package Com_Init is new Gcom (Auto_Init => Identity (True));
   package Com_Noinit is new Gcom (Auto_Init => Identity (False));
begin
   Assert (Com_Init.Initialized = True);
   Assert (Com_Noinit.Initialized = False);
end;

--# gcom.adb
-- /init_body/ l+ 0
-- /init_call/ l# x0
