package P1 is
   
   R : Boolean;
   
   procedure Andthen (A, B, C : Boolean);
   
   --   A B C   R
   -- 1 T T T   T
   -- 2 T T F   F
   -- 3 T F X   F
   -- 4 F X X   F

end;
