separate (Pkg)
function Compute_In_Stub (A, B : Boolean) return Integer is
begin
   if A and then B then
      return 2;
   else
      return 3;
   end if;
end Compute_In_Stub;
