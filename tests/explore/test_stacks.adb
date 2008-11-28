
with Stacks;

procedure Test_Stacks is
   package Integer_Stacks is new Stacks (Data_Type => Integer);
   use Integer_Stacks;

   X : Integer;
   S : Integer_Stacks.Stack (Capacity => 1);
begin
   Push (12, S);
   Pop (X, S);
end;

