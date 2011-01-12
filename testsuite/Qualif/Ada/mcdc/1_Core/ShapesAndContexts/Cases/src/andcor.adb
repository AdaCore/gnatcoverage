package body AndCOr is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      case B or else C is -- # orelse
	 when True  => return True;  -- # orTrue
	 when False => return False; -- # orFalse
      end case;
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      case A and then Orelse (B, C) is -- # andthen
         when True =>
            return True;               -- # returnTrue
         when False =>
            return False;              -- # returnFalse
      end case;
   end;

end;
