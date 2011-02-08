--  Contains routines with GOTO statements that transfer control out of block
--  statement paths

package GOTO_Statements_Block is

   subtype My_Range is Integer range 10 .. 20;

   function Compute
     (Par1 : Integer;
      Par2 : Integer;
      Par3 : Integer;
      Par4 : Integer)
      return Integer;
   --  Performs completely meaningless computations (see the body for the
   --  details), contains block statements with GOTO statements inside.

end GOTO_Statements_Block;
