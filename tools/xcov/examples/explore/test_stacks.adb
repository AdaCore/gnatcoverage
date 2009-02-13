-----------------
-- Test_Stacks --
-----------------

--  Exercise the Stacks abstraction

with Stacks, Support;

procedure Test_Stacks is
   package Integer_Stacks is new Stacks (Data_Type => Integer);
   use Integer_Stacks;

   X : Integer;
   S : Integer_Stacks.Stack (Capacity => 1);
begin
   Push (12, S);
   Pop (X, S);
   if X /= 12 then
      raise Program_Error;
   end if;
end Test_Stacks;

