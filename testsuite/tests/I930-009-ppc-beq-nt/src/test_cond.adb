with Get_False;

procedure Test_Cond is
   function Cond (V : Integer) return Integer;
   pragma Import (C, Cond);
begin
   if Get_False then
      if Cond (1) /= 2 then
         raise Program_Error;
      end if;
   end if;
end;
