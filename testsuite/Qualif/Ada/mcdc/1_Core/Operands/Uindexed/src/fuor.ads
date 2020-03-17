pragma Ada_2012;

package FUOR is

   type Op_Name is (A, B);
   type Varray is array (Op_Name) of aliased Float;

   type Operands is tagged record
      Values : Varray;
   end record
     with Constant_Indexing => C_Indexing;

   function Orelse (Ops : Operands) return Boolean;
   function C_Indexing
     (X : aliased Operands'Class; Op : Op_Name)
     return Float is (X.Values (Op));

end;
