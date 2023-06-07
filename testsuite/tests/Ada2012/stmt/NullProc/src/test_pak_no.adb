with Pak;
procedure Test_Pak_No is
begin
   null;
end Test_Pak_No;

--# pak.ads
--  /npc/   l- ## s-
--  /npp/   l- ## s-
--  /npb/   l- ## s-

--# pak.adb
--  /npc/   l- ## s-
--  /npp/   l- ## s-
--  /npb/   l- ## s-
--
--%opts: --trace-mode=bin %cargs:-O1
--  =/npp/  l. ## 0
