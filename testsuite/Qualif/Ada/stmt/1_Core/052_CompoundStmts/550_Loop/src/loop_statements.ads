--  Contains routines with different forms of LOOP statements in different
--  kinds of program units. All the  routines in this package use loop
--  statements with control statements that transfer control out of the loop.

with LOOP_Statements_Support; use LOOP_Statements_Support;
package LOOP_Statements is

   function All_Less_Then (L : Level; S  : Sample) return Boolean;
   --  Whether all the values in sample S are lower than L. The implementation
   --  uses FOR loop

   procedure Find_Last_Char
     (Res : out Natural;
      Str :     String;
      Ch  :     Character);
   --  Sets Res to the index of last character in Str that is equal to Ch. Sets
   --  Res to 0 if Str does not contain Ch. The implementation uses reverse FOR
   --  loop.

   generic
      type T is range <>;
   function Factorial (N : T) return T;
   --  Computes N! The implementation uses unconditional loop. (If the argument
   --  is less then 0, returns 1)

   generic
      Limit : Level;
   procedure Sum_First_Under_Limit
      (Res : out Integer;
       Arg :     Big_Sample);
   --  Computes in Res the sum of the first elements of Arg that are less then
   --  Limit. Stops the computation when the first element that are equal to or
   --  less then Limit is encountered. If the first element of Arg is equal to
   --  or greater then Limit, sets Res to 0. The implementation uses WHILE
   --  loop.

end LOOP_Statements;

