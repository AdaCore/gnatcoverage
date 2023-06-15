with Checks;

procedure Test_Assert is
   X : Integer := 12;
   pragma Volatile (X);
begin
   Checks.Assert (X = 12);
end;


