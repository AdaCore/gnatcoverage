package body Actions is
   
   Value : Integer := 0;
   pragma Volatile (Value);
   
   procedure Process_Positive (X : Integer) is
   begin
      if X <= 0 then  -- # test_pos
         raise Constraint_Error; -- # unreach_pos
      end if;
      Value := Value + X; -- # do_pos
   end;
   
   procedure Process_Negative (X : Integer) is
   begin
      if X >= 0 then -- # test_neg
         raise Constraint_Error; -- # unreach_neg
      end if;
      Value := Value + X; -- # do_neg
   end;
   
   procedure Process_Zero (X : Integer) is
   begin
      if X /= 0 then -- # test_zero
         raise Constraint_Error; -- # unreach_zero
      end if;
   end;
   
   procedure Process (X : Integer; Action : Action_Access) is
   begin
      Action.all (X);
   end;
end;
