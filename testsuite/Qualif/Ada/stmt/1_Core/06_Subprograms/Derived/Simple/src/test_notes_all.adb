with Notes; use Notes;

-- Exercise all the valid cases

procedure Test_Notes_ALL is
   Dtn  : DTNE;
   Dfn  : DFNE;
   Myid : Nid;
begin
   Myid := Id (Dtn);
   Myid := Id (Dfn);
end;

--# notes.adb
--  /cnote/ l- ## s-
--  /dtne/  l+ ## 0
--  /dfne/  l+ ## 0

