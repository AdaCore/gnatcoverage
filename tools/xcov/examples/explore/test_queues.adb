-----------------
-- Test_Queues --
-----------------

with Queues;

procedure Test_Queues is

   package Integer_Queues is new Queues (Data_Type => Integer);
   use Integer_Queues;

   X : Integer;
   Q : Integer_Queues.Queue (Capacity => 3);
begin
   if not Empty (Q) then
      raise Program_Error;
   end if;
   begin
      Pop (X, Q);
      raise Constraint_Error;
   exception
      when Program_Error => null;
   end;

   Push (1, Q);
   Push (2, Q);
   Push (3, Q);

   begin
      Push (4, Q);
      raise Constraint_Error;
   exception
      when Program_Error => null;
   end;

   Pop (X, Q);
   if X /= 1 then
      raise Program_Error;
   end if;
   Pop (X, Q);
   if X /= 2 then
      raise Program_Error;
   end if;
   Pop (X, Q);
   if X /= 3 then
      raise Program_Error;
   end if;
end;

