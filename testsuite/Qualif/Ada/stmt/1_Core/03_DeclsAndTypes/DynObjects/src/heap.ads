package Heap is
   
   --  This unit is devised to expose services making use of heap allocation
   --  in object declarations.

   type Integer_Access is access Integer;
   function Ptr_To_Abs (X : Integer) return Integer_Access;
   
   Global : Integer_Access := new Integer'(15); -- # global-decl
end;
