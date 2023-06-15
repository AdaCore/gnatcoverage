procedure For_Loop is
   X : Integer := 0;
   procedure Inc_X (N : Integer) is
   begin
      for I in 1 .. N loop
         X := X + 1;
      end loop;
   end;
begin
   Inc_X (0);
   if X /= 0 then
      raise Program_Error;
   end if;
end;
