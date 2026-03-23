procedure Main is
   function F (A, B : Boolean) return Boolean is
   begin
      if A and then B then
         return True;
      else
         return False;
      end if;
   end F;

   C : Boolean := F (False, True);
   D : Boolean := F (True, True);
begin
   null;
end Main;
