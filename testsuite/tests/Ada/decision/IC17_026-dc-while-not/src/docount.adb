with Counters; use Counters;

procedure Docount is
   C : aliased Counter := (Value => 5);
   Cref : Counter_Access := C'Unchecked_Access;
begin
   while not Zerop (Cref) loop -- # eval
      Dec (Cref);
   end loop;
end;
