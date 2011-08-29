package body Value is

   Temp : Boolean;

   function F (X : Boolean) return Boolean is
   begin
      Temp := X;   -- # eval :o/0:
      return Temp; -- # returnVal
   end;
end;
