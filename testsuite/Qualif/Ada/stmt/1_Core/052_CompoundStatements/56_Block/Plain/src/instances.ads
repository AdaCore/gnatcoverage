-- Provide functional instances of generics to be exercised

with Block_Statements;

package Instances is

   function My_Factorial is
      new Block_Statements.Factorial (Integer);

   procedure My_Swap is
      new Block_Statements.Swap_G (Integer);
end;
