package body AndCor is
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      loop
         exit when B or else C; -- # orelse :o/d:
	 return False; -- # orFalse
      end loop;
      return True;     -- # orTrue
   end;
   
   function F (A, B, C : Boolean) return Boolean is
   begin
      loop
         exit when A and then Orelse (B, C);  -- # andthen :o/d:
         return False;                        -- # returnFalse
      end loop;
      return True;                            -- # returnTrue
   end;
end;
