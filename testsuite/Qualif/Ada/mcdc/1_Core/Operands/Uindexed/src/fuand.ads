pragma Ada_2012;

-- In this instance, we exercise variable user indexing
-- yielding Integer values.

package FUAND is
   
   type Op_Name is (A, B);   
   type Varray is array (Op_Name) of aliased Integer;
   
   type Operands is tagged record
      Values : Varray;
   end record
     with Variable_Indexing => V_Indexing;
   
   type Integer_Ref (Ref : access Integer) is
     null record with Implicit_Dereference => Ref;
  
   function V_Indexing
     (X : aliased in out Operands; Op : Op_Name)
     return Integer_Ref is (Ref => X.Values (Op)'Access);

   function Andthen (Ops : Operands) return Boolean;
end;
