package body Pkg is
   function Compute (A, B : Boolean) return Integer is
   begin
      if A and then B then
         return 2;
      else
         return 3;
      end if;
   end Compute;

   function Compute_In_Stub (A, B : Boolean) return Integer is separate;
end Pkg;
