with Hello; use Hello;

procedure Test_Casual_Someone is
begin
   Say_Hello ("Jerome", Formal => False);
end;

--# hello.adb
--  /test_nobody/  l! ## dT-
--  /nobody/       l- ## s-
--  /someone/      l+ ## 0
--  /test_formal/  l! ## dT-
--  /formal/       l- ## s-
--  /casual/       l+ ## 0
