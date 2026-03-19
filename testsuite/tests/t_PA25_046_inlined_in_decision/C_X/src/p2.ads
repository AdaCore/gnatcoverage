package P2 is
   R : Boolean;
   CX : Integer := 0;
   procedure Combine (C : Boolean; X : Integer);

   -- V#  C  X>10  R
   -- 1   F  F     F
   -- 2   F  T     T
   -- 3   T  X     T

end;
