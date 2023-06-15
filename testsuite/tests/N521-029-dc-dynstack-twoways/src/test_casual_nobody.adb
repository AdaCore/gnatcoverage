with Hello; use Hello;

procedure Test_Casual_Nobody is
begin
   Say_Hello ("", Formal => False);
end;

--# hello.adb
--  /test_nobody/  l! ## dF-
--  /nobody/       l+ ## 0
--  /someone/      l- ## s-
--  /test_formal/  l! ## dT-
--  /formal/       l- ## s-
--  /casual/       l+ ## 0

