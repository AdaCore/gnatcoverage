package body Andthen is

   type Custom is new Boolean;

   function Make_Custom (X : Boolean) return Boolean is
   begin
      return X; -- # mkc
   end Make_Custom;

   function And_Then_Custom_Internal (A, B : Custom) return Custom is
   begin
      if A and then B then  -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

   function And_Then_Custom (A, B : Boolean) return Boolean is
   begin
      return Boolean (And_Then_Custom_Internal (Custom (A), Custom (B))); -- # call
   end And_Then_Custom;

end;
