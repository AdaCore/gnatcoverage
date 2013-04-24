procedure Andnot (A, B : Boolean; E : out Boolean) is
begin
   E :=             -- # Statementmark
     A              -- # Linemark
     and then       -- # Linemark
     not B;         -- # Linemark
end;
