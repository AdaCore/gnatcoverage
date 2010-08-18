with Support, Services; use Support, Services;

procedure Test_Serv_BA is
begin
   Assert (Services.Services (Event'(Cat => B), Object'(Cat => A)) = False);
end;

--# services.adb
--  /checkE/    l+ 0
--  /outEserv/  l+ 0
--  /checkO/    l- s-
--  /outOserv/  l- s-
--  /outOk/     l- s-
