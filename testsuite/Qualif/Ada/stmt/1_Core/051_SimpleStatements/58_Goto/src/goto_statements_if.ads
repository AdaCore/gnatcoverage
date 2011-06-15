--  Contains routines with GOTO statements that transfer control out of IF
--  statement paths

package GOTO_Statements_If is

   procedure Update
     (Arg  : in out Integer;
      Par1 : in out Integer;
      Par2 : in out Integer;
      Par3 :        Integer;
      Par4 :        Integer);
   --  Performs completely meaningless computations (see the body for the
   --  details), contains IF statement with GOTO statements inside.

end GOTO_Statements_If;
