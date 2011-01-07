package body AndCor is
   
   type My_Type (Value : Boolean) is null record;
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # orelse
   end;
   
   function F (A, B, C : Boolean) return Boolean is
      R : My_Type (Value => A and then Orelse (B, C)); -- # andthen
   begin
      return R.Value; -- # returnValue
   end;
end;
