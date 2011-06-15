--  Contains routines with GOTO statements in straight-line statement block.

package GOTO_Statements_Straight is

   generic
      type T is range <>;
   function Update_G (N : T) return T;
   --  If N < 0 then returns -N. If N = 0 then returns 1. If N > 0 then returns
   --  N ** 2.

end GOTO_Statements_Straight;
