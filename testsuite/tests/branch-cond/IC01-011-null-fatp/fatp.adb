package body Fatp is
   function Is_Null (S : String_Access) return Boolean is
   begin
      if S = null then
         return True;
      else
         return False;
      end if;
   end;
end;
