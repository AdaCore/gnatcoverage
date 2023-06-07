with Support; use Support;
with P; use P;

procedure Raise_Exception is
begin
   raise Force_Exception;       -- # force_exception
exception
   when Force_Exception =>
      raise Constraint_Error;            -- # raise
end Raise_Exception;
