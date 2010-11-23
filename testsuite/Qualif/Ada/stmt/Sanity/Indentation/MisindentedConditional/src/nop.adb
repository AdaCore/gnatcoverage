with Support; use Support;

procedure Nop (X : in out Integer) is
begin
   --  The two assignment statements below are conditioned but indented in
   --  such a way that a simple minded analyzer might believe they are not.
   --  The controlling conditions can never be True, so these statements are
   --  expected never to be covered.

   while Identity (X) > X loop
   X := X + 1;  -- # incX
   end loop;

      if Identity (X) /= X then
   X := X + 1;  -- # incX
      end if;
end;
