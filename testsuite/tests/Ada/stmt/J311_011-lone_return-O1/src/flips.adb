package body Flips is

   procedure Flip (X : in out integer) is
   begin
      X := - X; -- # flipx
   end;

   procedure Flip (B : in out Boolean) is
   begin
      B := not B; -- # flipb
   end;

end;
