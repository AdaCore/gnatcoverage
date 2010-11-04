package Overloads is
   procedure Flip (X : in out Integer);
   --  X := -X;

   procedure Flip (B : in out Boolean);
   --  B := not B;

   procedure Dispatch (Flipx, Flipb : Boolean);
   --  Call Flip (Integer/Boolean) iif Flipx/Flipb is True;
end;
