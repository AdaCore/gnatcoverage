with Support; use Support;

function Andidor (X, A, B : Boolean) return Boolean is
begin
   --  When this function is called with X = False, A or else B below is not
   --  evaluated eventhough the enclosing statement is covered.

   if X and then Identity (A or else B) then -- # eval
      return True;   -- # true
   else
      return False;  -- # false
   end if;
end;

