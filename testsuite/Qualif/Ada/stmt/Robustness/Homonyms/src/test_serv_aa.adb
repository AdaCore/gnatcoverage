with Support, Services; use Support, Services;

procedure Test_Serv_AA is
begin
   Assert (Services.Services (Event'(Cat => A), Object'(Cat => A)) = True);
end;

--# services.adb
--  /checkE/    l+ ## 0
--  /outEserv/  l- ## s-
--  /checkO/    l+ ## 0
--  /outOserv/  l- ## s-
--  /outOk/     l+ ## 0
