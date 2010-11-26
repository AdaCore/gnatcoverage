package body Fits is

   function Fits1 (S, Key : String) return Boolean is
   begin
      if Key'Length <= S'Length then  -- # single
         return True;   -- # returnTrue
      else
         return False;  -- # returnFalse
      end if;
   end;

   function Fits2 (S, Key : String) return Boolean is
   begin
      if Key'Length < S'Length or else Key'Length = S'Length then -- # orelse
         return True;   -- # returnTrue
      else
         return False;  -- # returnFalse
      end if;
   end;
end;
