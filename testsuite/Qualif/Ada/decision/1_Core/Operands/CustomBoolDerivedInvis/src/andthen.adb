package body Andthen is

   function Make_Custom (X : Boolean) return Boolean is
   begin
      return X; -- # mkc
   end Make_Custom;

   function And_Then_Custom (A, B : Boolean) return Boolean is
   begin
      if Usedef.Make_Custom (A).Comp and then Usedef.Make_Custom (B).Comp then  -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;

end;
