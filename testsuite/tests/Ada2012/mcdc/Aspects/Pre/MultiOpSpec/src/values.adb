package body Values is
   
   function Plus (A, B : Int) return Integer is
   begin
      return A.Value + B.Value;
   end;

end;
