pragma Ada_2012;
pragma Assertion_Policy (Invariant => Disable);

package Ops is
   
   type T_Value is tagged private
     with Type_Invariant'Class => I_Check(T_Value);
     
   function I_Check (V : T_Value) return Boolean;
     
   type T_Int (UB : Integer) is new T_Value with Private;
   
   procedure Set (I : in out T_Int; V : Integer; Count : Boolean);

private
   
   type T_Value is tagged record
      N_Sets : Integer := 0;
   end record;
   
   type T_Int (UB : Integer) is new T_Value with record
      Value : Integer := UB;
   end record with
     Type_Invariant => (T_Int.Value <= T_Int.UB);
   
end;
