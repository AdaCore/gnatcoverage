with Notes; use Notes;

-- Exercise the DF case only

procedure Test_Notes_DF is
   Myn  : DFNE;
   Myid : Nid;
begin
   Myid := Id (Myn);
end;

--# notes.adb
--  /cnote/ l- ## s-
--  /dtne/  l- ## s-
--  /dfne/  l+ ## 0
--
--%opts: --trace-mode=bin
--  /no-code/  l. ## 0
--
--%opts: --trace-mode=src
--  /no-code/  l- ## s-
