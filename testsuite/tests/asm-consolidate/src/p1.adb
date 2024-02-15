with Stuff1, Lib;

procedure P1 is
   X : Integer := 7;
   pragma Volatile (X);
begin
   Stuff1.Run;
   Lib.Adjust (X);
end;
