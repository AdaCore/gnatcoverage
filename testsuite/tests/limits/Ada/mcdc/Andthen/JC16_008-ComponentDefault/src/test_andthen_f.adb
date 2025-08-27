with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, True) = False);
   Assert (And_Then (True, False) = False);
end;

--# andthen.adb
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
--
--%opts: --trace-mode=bin
--  /andthen/  l. ## 0
--
--%opts: --trace-mode=src
--  /andthen/  l! ## eT-
