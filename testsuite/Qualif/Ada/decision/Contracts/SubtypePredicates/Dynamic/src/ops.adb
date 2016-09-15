
package body Ops is
   
   procedure Check (P : T_Pair) is
   begin
      if P.X > 0 then -- # test_pos
         N_Positives := N_Positives + 1; -- # pos
      end if;
   end;

end;
