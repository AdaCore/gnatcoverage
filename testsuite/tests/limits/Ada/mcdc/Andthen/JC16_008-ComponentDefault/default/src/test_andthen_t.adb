with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_T is
begin
   Assert (And_Then (True, True) = True);
end;

--# andthen.adb
--  /retTrue/  l+ ## 0
--  /retFalse/ l- ## s-
--  /retVal/   l+ ## 0
--
--%opts: --trace-mode=bin
--  /andthen/  l. ## 0
--
--%opts: --trace-mode=src
--  /andthen/  l! ## eF-
