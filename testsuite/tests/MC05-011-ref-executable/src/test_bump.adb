with Assert, Bump;

procedure Test_Bump is
   X : Integer := 12;
begin
   Bump (X);
   Assert (X = 13);
end;
