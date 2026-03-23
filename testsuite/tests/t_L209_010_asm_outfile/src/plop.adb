procedure Plop is
   X : Integer := 0;
   pragma Volatile (X);
begin
   X := X + 12;
end;
