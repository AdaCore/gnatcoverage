with Hello; use Hello;

procedure Test_Formal_Nobody is
begin
   Say_Hello ("", Formal => True);
end;

--# hello.adb
--  /test_nobody/  l! ## dF-
--  /nobody/       l+ ## 0
--  /someone/      l- ## s-
--  /test_formal/  l! ## dF-
--  /formal/       l+ ## 0
--  /casual/       l- ## s-
