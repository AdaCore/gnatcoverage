package body Andthen_Variants is
   function And_Then_Subtype (A, B : Bool_Subtype) return Bool_Subtype is
   begin
      if A and then B then  -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;
   function And_Then_Type (A, B : Bool_Type) return Bool_Type is
   begin
      if A and then B then  -- # eval
         return True;  -- # true
      else
         return False; -- # false
      end if;
   end;
end;
