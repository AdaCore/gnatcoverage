package body Ops is
   
   function I_Check (V : T_Value) return Boolean is
   begin
      return V.N_Sets >= 0; -- # check
   end;
   
   procedure Set (I : in out T_Int; V : Integer; Count : Boolean) is
   begin
      I.Value := V; -- # stmt
      
      if Count then -- # test_count
         I.N_Sets := I.N_Sets + 1; -- # count
      end if;
   end;
   
end;
