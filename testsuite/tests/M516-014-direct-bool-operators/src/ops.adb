package body Ops is
   
   procedure Eval (A, B : Boolean) is
   begin
      if A and B then        -- # eval-and
         N_And := N_And + 1; -- # and-true
      end if;
      if A or B then         -- # eval-or
         N_Or := N_Or + 1;   -- # or-true
      end if;
      if A xor B then        -- # eval-xor
         N_Xor := N_Xor + 1; -- # xor-true
      end if;
   end;
end;
     

  
