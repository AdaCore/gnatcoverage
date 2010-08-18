package body Andthen_Variants is
   function And_Then_Subtype (A, B : Bool_Subtype) return Bool_Subtype is
   begin
      return A and then B;  -- # evaluate
   end;
   function And_Then_Type (A, B : Bool_Type) return Bool_Type is
   begin
      return A and then B;  -- # evaluate
   end;
end;
