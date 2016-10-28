pragma Ada_2012;

package body Ops is
   
   function V_Indexing
     (X : aliased in out Composite_V; Op : Op_Name) return Integer_Ref
   is 
   begin
      case Op is -- # indexing
	 when OP_A => return (Ref => X.A'Access); -- # op_a
	 when Op_B => return (Ref => X.B'Access); -- # op_b
      end case;
   end;
   
   function C_Indexing
     (X : aliased Composite_C'Class; Op : Op_Name) return Integer
   is 
   begin
      case Op is -- # indexing
	 when OP_A => return X.A; -- # op_a
	 when Op_B => return X.B; -- # op_b
      end case;
   end;
end;
	  
      
