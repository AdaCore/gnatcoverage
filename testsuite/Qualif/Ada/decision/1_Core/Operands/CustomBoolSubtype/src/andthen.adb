package body Andthen is
   function Make_Custom (X : Boolean) return Custom is
   begin
      return X; -- # mkc
   end Make_Custom;
   function And_Then_Custom (A, B : Custom) return Custom is
   begin
      if A and then B then  -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;
end;
