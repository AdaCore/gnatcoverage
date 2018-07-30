--  Contains routines with different forms of block statements in different
--  kinds of program units.

package Block_Statements is

   type Sample is array (Natural range <>) of Integer;

   generic
      type T is private;
   procedure Swap_G (L, R : in out T);
   --  Swaps L and R. Uses block statement with declaration sequence

   procedure Swap_Max_Min (Arg : in out Sample);
   --  Swaps maximal and minimal elements of Arg. Does nothing if
   --  Arg'Length <= 1. Uses block statement with declaration sequence.

   generic
      type T is range <>;
   function Factorial (N : T) return T;
   --  Computes N!. Returns 0 if N < 0. Returns T'Last if the result is too
   --  big and Constraint_Error is raised. Used block statement with no
   --  declarative part and with exception handler.

   function Sum_Min_Max (Arg : Sample) return Integer;
   --  Returns the sum of minimal and maximal elements of the argument. Returns
   --  0 for null array. Uses nested block statements with declaration sequence
   --  each.

end Block_Statements;

