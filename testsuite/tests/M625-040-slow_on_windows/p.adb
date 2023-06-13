
procedure P is
   X : Integer := 0;
   pragma Volatile (X);
   
   N_Increments : Integer := 5000;
   pragma Volatile (N_increments);
   
begin
   for I in 1 .. N_Increments loop
      X := X + 1;
   end loop;
   if X /= N_Increments then
      raise Program_Error;
   end if;
end;
