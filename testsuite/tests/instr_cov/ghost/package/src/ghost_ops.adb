package body Ghost_Ops is
   function Bumpable (X : Integer) return Boolean is
   begin
      if X < Integer'last then -- # ghost-if
         return True;          -- # ghost-then
      else
         return False;         -- # ghost-else
      end if;
   end;
end;
