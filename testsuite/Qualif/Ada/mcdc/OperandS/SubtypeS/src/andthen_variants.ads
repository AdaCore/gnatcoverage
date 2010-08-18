package Andthen_Variants is
   subtype Bool_Subtype is Boolean;
   function And_Then_Subtype (A, B : Bool_Subtype) return Bool_Subtype;
   
   type Bool_Type is new Boolean;
   function And_Then_Type (A, B : Bool_Type) return Bool_Type;
end;
