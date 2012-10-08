with Support, Heap; use Support, Heap;

procedure Test_Heap_Pos is
   Ptr : Integer_Access := Ptr_To_Abs (5);
begin
   Assert (Ptr.all = 5);
end;

--# heap.ads
--  /global-decl/ l+ ## 0

--# heap.adb
--  /on-call/  l+ ## 0
--  /pos-decl/ l+ ## 0
--  /pos-stmt/ l+ ## 0
--  /neg-decl/ l- ## s-
--  /neg-stmt/ l- ## s-

