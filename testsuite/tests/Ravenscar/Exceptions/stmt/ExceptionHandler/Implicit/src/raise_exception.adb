with Support; use Support;
with P; use P;

procedure Raise_Exception is
   X: Integer;
   pragma Unsuppress (All_Checks);
begin
   raise Force_Exception;       -- # force_exception
exception
   when Force_Exception =>
      X := Identity(5)/Identity (0);                 -- # raise
end Raise_Exception;
