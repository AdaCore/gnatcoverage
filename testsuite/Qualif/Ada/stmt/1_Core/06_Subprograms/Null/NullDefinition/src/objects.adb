package body Objects is
   procedure Local_Probe (O : T_Object) is null; -- # local
   
   procedure Process (O : T_Object; Local : Boolean) is
   begin
      if Local then -- # test
         Local_Probe (O); -- # local
      else 
         Probe (O); -- # global
      end if;
   end;
end;
