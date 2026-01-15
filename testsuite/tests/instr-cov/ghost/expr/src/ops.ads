package Ops is

   A, B : Boolean;
   procedure Monitor (A, B : Boolean);

   N_Checks : Natural := 0;
   -- Number of times Monitor was called
end;
