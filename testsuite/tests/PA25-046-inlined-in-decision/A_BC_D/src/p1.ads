package P1 is
   
   R : Boolean;
   
   function Orelse (B, C : Boolean) return Boolean;
   pragma Inline (Orelse);

   procedure Combine (A, B, C, D : Boolean);
   
   --   A && (B C or) && D   R
   -- 1 F     X X X      X   F
   -- 2 T     T X T      F   F
   -- 3 T     F T T      F   F
   -- 4 T     F F F      X   F
   -- 5 T     T X T      T   T
   -- 6 T     F T T      T   T

end;
