with Lib;

procedure Test_Exp is
begin
   if Lib.Exp (1, 1) /= 1 then
      raise Program_Error;
   end if;
end Test_Exp;
