
procedure P is
   X : Integer := 0;
   pragma Volatile (X);
begin
   X := X + 1;
end;
