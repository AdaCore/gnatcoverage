with Notes; use Notes;

-- Exercise the DT case only

procedure Test_Notes_DT is
   Myn  : DTNE;
   Myid : Nid;
begin
   Myid := Id (Myn);
end;

--# notes.adb
--  /cnote/ l- ## s-
--  /dtne/  l+ ## 0
--  /dfne/  l- ## s-
--
--%opts: --trace-mode=bin
--  /no-code/  l. ## 0
--
--%opts: --trace-mode=src
--  /no-code/  l- ## s-
