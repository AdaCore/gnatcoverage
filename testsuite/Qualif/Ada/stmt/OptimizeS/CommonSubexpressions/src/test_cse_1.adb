with Support, CSE; use Support, CSE;

procedure Test_Cse_1 is
   X : Integer := 0;
begin
   Bump (X);
   Assert (X = 1);
end;

--# cse.adb
--  /dobump/   l+ 0
--  /loop/     l+ 0
--  /noreach/  l- s-
