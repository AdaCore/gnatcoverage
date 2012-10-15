with Support, Heap; use Support, Heap;

procedure Test_Heap_Neg is
   Ptr : Integer_Access := Ptr_To_Abs (-7);
begin
   Assert (Ptr.all = 7);
end;

--# heap.ads
--  /global-decl/ l+ ## 0

--# heap.adb
--  /on-call/  l+ ## 0
--  /pos-decl/ l- ## s-
--  /pos-stmt/ l- ## s-
--  /neg-decl/ l+ ## 0
--  /neg-stmt/ l+ ## 0

