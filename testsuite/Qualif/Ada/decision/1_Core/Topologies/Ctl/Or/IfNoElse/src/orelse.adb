package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      if A or else B then  -- # orelse
         return True;      -- # retTrue
      end if;
      return False;     -- # retFalse
   end;
end;



