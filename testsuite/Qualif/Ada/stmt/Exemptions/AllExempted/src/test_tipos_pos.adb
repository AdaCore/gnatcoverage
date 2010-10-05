with Support, Tipos; use Support;

procedure Test_Tipos_Pos is
begin
   Assert (Tipos (5) = 10);
end;

--# tipos.adb
--  /test/  l+ 0
--  /pos/   l+ 0
--  /other/ l- %s-
