with Last_Chance_Handler;

procedure Test_Cond is
   function Cond (V : Integer) return Integer;
   pragma Import (C, Cond);
begin
   if Cond (0) /= 120 then
      raise Program_Error;
   end if;
end;
