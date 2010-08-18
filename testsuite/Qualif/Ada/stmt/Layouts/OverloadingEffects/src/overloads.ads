package Overloads is
   procedure Flip (X : in out Integer);
   procedure Flip (B : in out Boolean);

   procedure Dispatch (Flipx, Flipb : Boolean);
end;
