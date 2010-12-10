package body Flip is
   function F (X : Boolean) return Boolean is
   begin
      case not X is                    -- # evaluate
         when True  => return True;    -- # returnTrue
         when False => return False;   -- # returnFalse
      end case;
   end;
end;
