with Pak;
procedure Test_Pak_Call is
begin
   Pak.Call;
end Test_Pak_Call;

--# pak.ads
--  /npc/   l+ ## 0
--  /npp/   l- ## s-
--  /npb/   l+ ## 0

--# pak.adb
--  /npc/   l+ ## 0
--  /npp/   l- ## s-
--  /npb/   l+ ## 0
--
--%opts: --trace-mode=bin %cargs:-O1
--  =/npp/  l. ## 0
