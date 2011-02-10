package body Checks is
   procedure Check (Cond : Boolean) is
   begin
      if Cond then                        -- # eval
         N_Checks_Ok := N_Checks_Ok + 1;  -- # incOK
      end if;
      N_Checks := N_Checks + 1;           -- # incAll
   end;
end;
