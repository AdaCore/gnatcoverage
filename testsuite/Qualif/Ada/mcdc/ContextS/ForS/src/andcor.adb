package body AndCOr is

   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # orelse
   end;

   function F (A, B, C : Boolean) return Boolean is
      type Bmap is array (Boolean) of Boolean;  -- # decl
      Touched : Bmap := (others => False);  -- # decl
   begin
      for Value in False .. (A and then Orelse (B, C)) loop -- # andthen
         Touched (Value) := True;     -- # returnValue
      end loop;
      return Touched (True);  -- # returnValue
   end;
end;
