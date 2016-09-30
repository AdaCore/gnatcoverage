pragma Ada_2012;

package FUOR is
   
   type Op_Name is (Op_A, Op_B);   
   
   type Operands is tagged record
      A, B : Float;
   end record
     with Constant_Indexing => C_Indexing;
   
   function C_Indexing
     (X : aliased Operands'Class; Op : Op_Name)
     return Float is 
      (case Op is
         when Op_A => X.A,
         when Op_B => X.B);
        
   function Orelse (Ops : Operands) return Boolean;
end;
