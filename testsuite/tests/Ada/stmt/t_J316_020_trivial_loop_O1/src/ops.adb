package body Ops is

   procedure Probe (N : Natural) is
      Nprobes : Natural := 0;  -- # decl
   begin
      while Nprobes /= N loop    -- # loop
         Latch := IOreg;         -- # probe
         Nprobes := Nprobes + 1; -- # probe
      end loop;
   end;
end;
