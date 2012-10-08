with Support, Heap; use Support, Heap;

procedure Test_Heap_All is
   Px : Integer_Access := Ptr_To_Abs (5);
   Py : Integer_Access := Ptr_To_Abs (-7);
begin
   Assert (Px.all = 5);
   Assert (Py.all = 7);
end;

--# heap.ads
--  /global-decl/ l+ ## 0

--# heap.adb
--  /on-call/  l+ ## 0
--  /pos-decl/ l+ ## 0
--  /pos-stmt/ l+ ## 0
--  /neg-decl/ l+ ## 0
--  /neg-stmt/ l+ ## 0

