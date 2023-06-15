with Lib;
procedure P2 is
   X : Integer := -12;
   pragma Volatile (X);
begin
   Lib.Adjust (X);
end;

  
