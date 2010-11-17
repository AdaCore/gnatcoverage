with Support, Com; use Support, Com;

--  Functional package is withed - elab body is covered with auto_init
--  off, so call to init gets exempted

procedure Test_Com is
begin
   Assert (Com.Initialized = False);
end;

--# com.adb
-- /init_body/ l- s-
-- /init_call/ l* x+
