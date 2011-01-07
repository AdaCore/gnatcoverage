with Support, Com; use Support, Com;

--  The functional package is withed - it's elaboration body is covered with
--  auto_init off, so the init body is not executed and the call to init gets
--  exempted.

procedure Test_Com is
begin
   Assert (Com.Initialized = False);
end;

--# com.adb
-- /init_body/ l- s-
-- /init_call/ l* x+
