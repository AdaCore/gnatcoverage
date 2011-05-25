package body Andthen is

   function And_Then (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # retVal
      Touched : Bmap := (others => False);  -- # retVal
   begin
      for Value in False .. (A and then B) loop -- # andthen
         Touched (Value) := True;              -- # retVal
      end loop;
      return Touched (True);  -- # retVal
   end;
end;



