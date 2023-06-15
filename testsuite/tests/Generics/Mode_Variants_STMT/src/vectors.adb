package body Vectors is
   
   function Do_Reset (I : Integer) return Integer is
   begin
      return 0; -- # do_reset
   end;
   
   function Do_Bump (I : Integer) return Integer is
   begin
      return I + 1; -- # do_bump
   end;
   
   type Operator is access function (X : Integer) return Integer;
   
   procedure Iterate_On (V : in out Vector; Op : Operator) is
   begin
      for I in V'Range loop   -- # iterate
         V(I) := Op (V (I));  -- # iterate
      end loop;
   end;
     
   procedure Apply (Op : Opkind; V : in out Vector) is
   begin
      case Op is -- # apply_op
         when Reset => Iterate_On (V, Do_Reset'Access);   -- # apply_reset
         when Bump => Iterate_On (V, Do_Bump'Access); -- # apply_bump
      end case;
   end;
   
end;

        
