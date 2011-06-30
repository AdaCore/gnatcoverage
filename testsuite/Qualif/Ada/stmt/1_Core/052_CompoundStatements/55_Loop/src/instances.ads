-- Provide functional instances of generics to be exercised

with Loop_Statements;

package Instances is
   function My_Factorial is
      new Loop_Statements.Factorial (Natural);

   procedure My_Sum_First_Under_Limit is
      new Loop_Statements.Sum_First_Under_Limit (10);
end;
