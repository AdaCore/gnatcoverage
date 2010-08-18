with Support; use Support;

procedure Nop (X : in out Integer) is
begin
   while Identity (X) > X loop
   X := X + 1;  -- # XnoCov
   end loop;
      if Identity (X) /= X then
   X := X + 1;  -- # XnoCov
      end if;
end;
