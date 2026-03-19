procedure Volp is
   X : Integer := 10;
   pragma Volatile (X);
begin
   if X /= 10 then
      raise Program_Error;
   end if;
end;
