procedure MyInc is
   X : Integer := 1;
   pragma Volatile (X);
begin
   X := X + 1;
end;
