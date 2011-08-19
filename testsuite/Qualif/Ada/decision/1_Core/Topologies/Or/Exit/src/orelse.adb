package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      loop
         exit when A or else B;  -- # orelse :o/d:
         return False;      -- # retFalse
      end loop;
      return True;     -- # retTrue
   end;
end;



