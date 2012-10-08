package body Heap is
   
   function Ptr_To_Abs (X : Integer) return Integer_Access is
   begin
      if X > 0 then -- # on-call
         declare
            Ptr : Integer_Access := new Integer'(X); -- # pos-decl
         begin
            return Ptr; -- # pos-stmt
         end;
      else
         declare
            Ptr : Integer_Access := new Integer'(-X); -- # neg-decl
         begin
            return Ptr; -- # neg-stmt
         end;
      end if;
   end;
end;
