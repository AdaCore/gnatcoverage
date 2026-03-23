with Support, Andthen; use Support, Andthen;

procedure Test_Single is
begin
   Assert (not And_Then (False, False));
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
--  /andthen/  l? ## d?
