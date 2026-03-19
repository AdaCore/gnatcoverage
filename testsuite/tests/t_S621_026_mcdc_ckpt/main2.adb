procedure Main2 is
   function And_Then (X, Y : Boolean) return Boolean is
   begin
      if X and then Y then
         return True;
      else
         return False;
      end if;
   end And_Then;

   Dummy : array (Positive range <>) of Boolean :=
     (And_Then (False, False),
      And_Then (True, True));
begin
   null;
end Main2;
