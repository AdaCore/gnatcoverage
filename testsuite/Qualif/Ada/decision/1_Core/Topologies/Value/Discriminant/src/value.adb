with Types;

package body Value is
   function F (X : Boolean) return Boolean is
      My_B : Types.Bool (Val => X);   -- # eval :o/0:
   begin
      return My_B.Val; -- # returnVal
   end;
end;
