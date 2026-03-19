function P (S1, S2 : String; L : Integer) return Boolean is
begin
   pragma Warnings (Off, "index for * may assume lower bound of 1");
   if S1 (1 .. L) = S2 (1 .. L) then
      return True;
   else
      return False;
   end if;
   pragma Warnings (On, "index for * may assume lower bound of 1");
end;
