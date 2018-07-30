--  Contains routines with GOTO statements that transfer control out of CASE
--  statement paths

package GOTO_Statements_Case is

   subtype My_Range is Integer range 10 .. 20;

   procedure Update
     (Arg  : in out Integer;
      Par1 : in out Integer;
      Par2 : in out Integer;
      Par3 :        Integer;
      Par4 :        Integer);
   --  Performs completely meaningless computations (see the body for the
   --  details), contains CASE statement with GOTO statements inside.

end GOTO_Statements_Case;
