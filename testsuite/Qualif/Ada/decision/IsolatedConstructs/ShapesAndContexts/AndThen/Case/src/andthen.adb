package body Andthen is
   function And_Then (A, B : Boolean) return Boolean is
   begin
      case A and then B is  -- # andthen
         when True => return True;      -- # retTrue
         when False => return False;    -- # retFalse
      end case;
   end;
end;



