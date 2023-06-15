with Hello; use Hello;

procedure Test_Formal_Someone is
begin
   Say_Hello ("Jerome", Formal => True);
end;

--# hello.adb
--  /test_nobody/  l! ## dT-
--  /nobody/       l- ## s-
--  /someone/      l+ ## 0
--  /test_formal/  l! ## dF-
--  /formal/       l+ ## 0
--  /casual/       l- ## s-

