procedure P is
begin
   raise Constraint_Error;
exception
   when others => null;
end;
