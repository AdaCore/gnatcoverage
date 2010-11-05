package Ranges is
   procedure Check_Range (X, Min, Max : Integer; In_Range : out Boolean);
   --  Compute whether X is in the [Min, Max] integer range, checking
   --  first if X < Min, then if X > Max.
end;
