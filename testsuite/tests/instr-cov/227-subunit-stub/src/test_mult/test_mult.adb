with Lib;

procedure Test_Mult is
begin
   if Lib.Mult (1, 1) /= 1 then
      raise Program_Error;
   end if;
end Test_Mult;
