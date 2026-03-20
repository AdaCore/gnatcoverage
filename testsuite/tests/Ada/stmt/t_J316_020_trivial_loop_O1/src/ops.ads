package Ops is

   Latch, IOreg : Integer;
   pragma Volatile (IOreg);
   pragma Volatile (Latch);

   procedure Probe (N : Natural);
end;
