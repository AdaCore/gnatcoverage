pragma Assertion_Policy (Check);
pragma Ada_2022;

package Pkg is

   --  F1 always returns True. In its precondition:
   --
   --  * If X < 0, only the first condition is evaluated (to True).
   --  * Otherwise, the recursive call to F1 will trigger a new (nested)
   --    precondition evaluation.
   --
   --  The MCDC state variable used to be generated outside of F1. As a
   --  consequence, we had multiple evaluations of F1's precondition at the
   --  same time using the same MCDC state variable: the MCDC path index
   --  computed for each instance was incremented by each recursive call, and
   --  so in the end, the witness call for MCDC was targetting the wrong bits
   --  in the MCDC coverage buffer: bits for F2's decision.

   function F1 (X : Integer) return Boolean
   is (X in Integer)                         -- # f1-body
   with Pre => X < 0 or else F1 (X - 1);     -- # f1-pre

   function F2 (X, Y, Z : Integer) return Boolean
   is (X > 0 and then Y > 0 and then Z > 0); -- # f2-body

end Pkg;
