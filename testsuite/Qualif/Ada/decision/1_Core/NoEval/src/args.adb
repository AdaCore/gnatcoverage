package body Args is
   function Eval (A : Arg) return Boolean is
   begin
      if A.Raise_On_Eval then
         raise Program_Error;
      else
         return A.Value;
      end if;
   end;

   function Id (X : Num) return Num is
   begin
      return X;
   end;

end;


