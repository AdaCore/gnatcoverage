package body AndCor is
   
   type Expr (Value : Boolean) is null record; -- # typedecl
   
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return B or else C; -- # orelse :o/e:
   end;
   
   function F (A, B, C : Boolean) return Boolean is
      R : Expr (Value => A and then Orelse (B, C)); -- # andthen :o/e:
   begin
      return R.Value; -- # returnValue
   end;
end;
