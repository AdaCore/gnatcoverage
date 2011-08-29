with Types;

package body Value is
   function F (X : Boolean) return Boolean is
      My_B : Types.Bool := (Value => X);   -- # eval :o/0:
   begin
      return My_B.Value; -- # returnVal
   end;
end;
