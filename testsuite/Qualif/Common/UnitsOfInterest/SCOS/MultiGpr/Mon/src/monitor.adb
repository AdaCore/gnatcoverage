package body Monitor is
   procedure Diamond (A, B, C : Boolean) is
   begin
      if A and then (B or else C) then
	 Hit := Hit + 1;
      else
	 Miss := Miss + 1;
      end if;
   end;
end;
