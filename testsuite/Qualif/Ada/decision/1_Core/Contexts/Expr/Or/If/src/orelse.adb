package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      if A or else B then  -- # orelse
         return True;      -- # retTrue
      else
         return False;     -- # retFalse
      end if;
   end;
end;



