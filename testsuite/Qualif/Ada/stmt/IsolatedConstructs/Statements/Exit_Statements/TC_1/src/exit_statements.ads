--  Contains routines with different forms of EXIT statements in different
--  kinds of program units.

with EXIT_Statements_Support; use EXIT_Statements_Support;
package EXIT_Statements is

   generic
      type T is range <>;
   function Factorial (N : T) return T;
   --  Computes N! (if the argument is less then 0, returns 1). The
   --  implementation uses unconditional loop with two EXIT statements.

   procedure Update_Array_Sample
     (Arg   : in out Array_Sample;
      Limit :        Positive);
   --  Iterates through elements of Arg and computes the sum of elements. Stops
   --  iteration when the first negative element is encountered. Each
   --  (non-negative) element is replaced with the computed sum while this sum
   --  is less then Limit. If the sum is equal or greater then Limit,
   --  iteration is stopped. The implementation contains FOR loop with
   --  conditional and unconditional EXIT statements.

   function Compute_On_Matrix
     (M     : Matrix_Sample;
      Limit : Natural)
      return  Natural;
   --  Traverses the argument line-by-line, and computes the sum of its
   --  elements that is returned as the result. For an empty matrix, zero is
   --  returned. (The traversal and computation is stopped if one of the
   --  following conditions satisfies:
   --  - the visited element is negative (if the first element is negative, the
   --    returned result is 0);
   --  - the computes sum exceeds Limit
   --  The implementation uses nested loops with two EXIT statements in the
   --  inner loop, both of them exit the outer loop.

end EXIT_Statements;

