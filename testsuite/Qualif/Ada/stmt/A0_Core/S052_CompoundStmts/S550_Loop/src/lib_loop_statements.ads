-- Package featuring a loop controlled by a decision within
-- its body elaboration code.

package Lib_Loop_Statements is

   -- An array of integers, all zero for starters. A number of these
   -- are set to 1 in the package elab body. The number of values so
   -- initialized is controlled by a variable in a distinct package spec,
   -- provided by each testcase.
   Values : array (1 .. 8) of Integer := (others => 0);

   function N_Ones return Integer;
   --  Recompute the number of slots set by looking at the values

end;
