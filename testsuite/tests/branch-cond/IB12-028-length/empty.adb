function Empty (S : String) return Boolean is
begin
   if S'Length > 0 then
      return True;
   else
      return False;
   end if;
end;
