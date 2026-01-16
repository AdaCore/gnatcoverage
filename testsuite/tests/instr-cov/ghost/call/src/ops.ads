pragma Ada_2012;

package Ops is
   procedure Check_Bumpable (X : Integer);

   N_Checks : Natural := 0;
   -- Number of times Check_Bumpable was called

   Ghost_Check : Boolean with Ghost;
   -- Result of the last Check_Bumpable
end;
