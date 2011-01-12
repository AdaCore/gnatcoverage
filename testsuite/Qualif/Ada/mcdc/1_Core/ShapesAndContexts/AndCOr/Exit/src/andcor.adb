package body AndCor is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      loop
         exit when B or else C; -- # orelse
	 return False; -- # orFalse
      end loop;
      return True;     -- # orTrue
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      loop
         exit when A and then Orelse (B, C);  -- # andthen
         return False;                        -- # returnFalse
      end loop;
      return True;                            -- # returnTrue
   end;
end;
