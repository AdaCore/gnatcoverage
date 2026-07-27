procedure Main is
   X : Integer := 12;
begin
   if X < 0 then
      raise Program_Error;
   end if;
end Main;
