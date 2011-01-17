
package A1O1 is
   function F (A, B : Boolean) return Boolean;
   --  Evaluate A and then B as well as A or else B on the same line.
   --  Return the former.

   --  +--------+-----+           +--------+----+
   --  | A   B  | And |  Vec #    | A   B  | Or |
   --  +--------+-----+           +--------+----+
   --  | F  (F) |  F  |    0      | F   F  | F  |
   --  | F  (T) |  F  |    1      | F   T  | T  |
   --  | T   F  |  F  |    2      | T  (F) | T  |
   --  | T   T  |  T  |    3      | T  (T) | T  |
   --  +--------+-----+           +--------+----+

   --  Independence pairs:

   --  ip(A): 3,0 3,1             ip(A): 0,2 0,3
   --  ip(B): 3,2                 ip(B): 0,1
end;
