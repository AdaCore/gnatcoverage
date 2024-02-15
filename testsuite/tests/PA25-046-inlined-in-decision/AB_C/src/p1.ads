package P1 is

   R : Boolean;

   procedure Combine (A, B, C : Boolean);

   --   A B  and  C  R
   -- 1 F X  F    F  F
   -- 2 T F  F    F  F
   -- 3 T T  T    X  T

   -- 4 F X  F    T  T
   -- 5 T F  F    T  T

end;
