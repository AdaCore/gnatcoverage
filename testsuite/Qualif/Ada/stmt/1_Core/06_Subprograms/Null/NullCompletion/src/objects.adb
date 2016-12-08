package body Objects is
   procedure Probe (O : T_Object) is
      null; -- # test
   
   procedure Process (O : T_Object) is
      procedure Local_Probe (O : T_Object); -- # decl
      
      procedure Local_Probe (O : T_Object) is null; -- # test
      
   begin
      Local_Probe (O); -- # test
   end;
end;
