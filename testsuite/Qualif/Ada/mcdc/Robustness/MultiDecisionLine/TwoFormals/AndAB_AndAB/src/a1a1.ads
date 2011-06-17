
package A1A1 is
   procedure Process (A, B : Boolean);
   --  Evaluate A and then B twice on the same line

   --  +--------+-----+
   --  | A   B  | And |  Vec #
   --  +--------+-----+
   --  | F  (F) |  F  |    0
   --  | F  (T) |  F  |    1
   --  | T   F  |  F  |    2
   --  | T   T  |  T  |    3
   --  +--------+-----+

   --  Independence pairs:

   --  ip(A): 3,0 3,1
   --  ip(B): 3,2
end;
