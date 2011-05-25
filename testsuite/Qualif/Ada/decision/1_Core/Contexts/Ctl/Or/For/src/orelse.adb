package body Orelse is

   function Or_Else (A, B : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # retVal
      Touched : Bmap := (others => False);  -- # retVal
   begin
      for Value in False .. (A or else B) loop -- # orelse
         Touched (Value) := True;              -- # retVal
      end loop;
      return Touched (True);  -- # retVal
   end;
end;



