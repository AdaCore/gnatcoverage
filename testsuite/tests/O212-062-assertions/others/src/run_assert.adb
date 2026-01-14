procedure Run_Assert
is
   X : Integer := 0;                      -- # decl
begin
   --  This statement is never executed as Run_Assert is never called.
   pragma Assert (X /= 0 or else False);  -- # assert
end Run_Assert;
