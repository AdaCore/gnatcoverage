--  Contains routines with different forms of IF statements in different kinds
--  of program inits

package IF_Statements is
   function In_Range (X, Min, Max : Integer) return Boolean;
   --  Whether X is between MIN and MAX included. The body contains IF
   --  statement with one ELSIF and ELSE paths

   generic
      type T is range <>;
   procedure Set_Max
     (Res        : out T;
      Arg1, Arg2 :     T);
   --  Set Res to maximun from Arg1 and Arg2. The body contains IF statement
   --  without ELSE path and without ELSIF path
end IF_Statements;

