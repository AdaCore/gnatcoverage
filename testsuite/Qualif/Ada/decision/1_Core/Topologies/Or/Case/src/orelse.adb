package body Orelse is
   function Or_Else (A, B : Boolean) return Boolean is
   begin
      case A or else B is  -- # orelse :o/0:
	 when True  => return True;      -- # retTrue
	 when False => return False;     -- # retFalse
      end case;
   end;
end;



