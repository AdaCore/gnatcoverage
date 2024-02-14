with Support, Counters; use Support;

procedure Falseonly is

   function Pos (X : Integer) return Boolean is
   begin
      if X > 0 then                         -- # test
         Counters.Bump (Counters.Nthen);    -- # then
         return True;                       -- # then
      else
         Counters.Bump (Counters.Nelse);    -- # else
         return False;                      -- # else
      end if;
   end Pos;

begin
   Assert (Pos (-1) = False);
   Assert (Counters.Nthen = 0);
   Assert (Counters.Nelse = 1);
end;
