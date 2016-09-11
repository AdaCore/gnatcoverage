package body Objects is
   procedure Local_Probe (O : T_Object) is null; -- # test
   
   procedure Process (O : T_Object) is
   begin
      A_Probe := Local_Probe'Address; -- # test
   end;
end;
