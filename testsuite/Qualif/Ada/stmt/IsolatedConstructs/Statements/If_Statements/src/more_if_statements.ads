--  Contains routines with different forms of IF statements in different kinds
--  of program units. The body contains IF statement is an elaboration
--  statement sequence

package More_IF_Statements is
   pragma Elaborate_Body;

   Global_Var : Integer;

   procedure Set_Prime_Number
     (Res : out Natural;
      Num :     Natural);
   --  Sets Res to Nth prime number. If N > 10, sets Res to 0. The body
   --  contains IF statements with many ELSIF parts and no ELSE part

   generic
      type T is range <>;
   function Max_From_Two (Arg1, Arg2 : T) return T;
   --  Returns maximum from its arguments. The body contains IF statement with
   --  ELSE path and with no ELSIF path.

end More_IF_Statements;

