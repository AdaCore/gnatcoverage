with State; use State;

procedure P is
begin
   X := A and then B;             -- tree bdd

   Y := (A and then B) or else C; -- !tree bdd

   Z := (A or else B) and then C; -- !tree bdd
end;
