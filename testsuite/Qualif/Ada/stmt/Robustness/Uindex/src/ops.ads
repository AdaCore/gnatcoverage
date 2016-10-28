pragma Ada_2012;

package Ops is
   
   type Op_Name is (Op_A, Op_B);   
   
   type Composite_V is tagged record
      A, B : aliased Integer;
   end record with
     Variable_Indexing => V_Indexing;
   
   type Composite_C is tagged record
      A, B : aliased Integer;
   end record with
     Constant_Indexing => C_Indexing;
   
   type Integer_Ref (Ref : access Integer) is
     null record with Implicit_Dereference => Ref;
  
   function V_Indexing
     (X : aliased in out Composite_V; Op : Op_Name)
     return Integer_Ref;
   
   function C_Indexing
     (X : aliased Composite_C'Class; Op : Op_Name)
     return Integer;
   
end;
	 
