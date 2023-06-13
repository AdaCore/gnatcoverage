package body Cond_Raise is
   
   procedure Raise_If (Cond : Boolean) is
   begin
      if Cond then  -- # test_cond
         N_Raise := N_Raise + 1;  -- # raise
         raise Constraint_Error;  -- # raise
         
         --  Repeating # raise is done on purpose here, to let
         --  test drivers designate the set of lines/statements
         --  that all get to execute or not depending on Cond.
      end if;
      N_Past_Test := N_Past_Test + 1; -- # past_test
   end;

   procedure Check_For (Cond : Boolean) is
   begin
      Raise_If (Cond => Cond); -- # call
      N_Past_Call := N_Past_Call + 1; -- # past_call
   exception
      when Constraint_Error =>
         N_In_Handler := N_In_Handler + 1; -- # in_handler
   end;
end;


