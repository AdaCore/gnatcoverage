-- Provide functional instances of generics to be exercised

with If_Statements, More_IF_Statements;

package Instances is

   procedure My_Set_Max is
      new If_Statements.Set_Max (Integer);

   function My_Max_From_Two is
      new More_If_Statements.Max_From_Two (Integer);
end;

