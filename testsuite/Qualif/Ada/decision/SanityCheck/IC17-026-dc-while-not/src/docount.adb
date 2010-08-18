with Counters; use Counters;

procedure Docount is
   C : Counter_Access := new Counter'(Value => 5);
begin
   while not Zerop (C) loop -- # eval
      Dec (C);
   end loop;
end;
