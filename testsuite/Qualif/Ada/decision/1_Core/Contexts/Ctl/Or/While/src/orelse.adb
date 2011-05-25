package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      while A or else B loop  -- # orelse
         return True;      -- # retTrue
      end loop;
      return False;     -- # retFalse
   end;
end;



