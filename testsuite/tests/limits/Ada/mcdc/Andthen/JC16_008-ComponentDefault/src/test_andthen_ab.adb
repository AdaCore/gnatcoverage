with Support, Andthen ; use Support, Andthen;

procedure Test_Andthen_AB is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (True, False) = False);
   Assert (And_Then (False, True) = False);
end;

--# andthen.adb
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
--
--%opts: --trace-mode=bin
--  /andthen/  l. ## 0
--
--%opts: --trace-mode=src
--  /andthen/  l? ## e?
