package body Vars is
   procedure Raise_PE is
   begin
      raise Program_Error;  -- # call_raise
   end;
   
   procedure Post_Raise is
   begin
      N_Post_Raise := N_Post_Raise + 1; -- # post_raise
   end;
end;
