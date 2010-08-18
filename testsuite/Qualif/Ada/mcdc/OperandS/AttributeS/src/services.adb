package body Services is
   function Check_Not_Null (S : String_Access) return Boolean is
   begin
      return S /= null;
   end Check_Not_Null;

   function Starts (S : String_Access; C : Character) return Boolean is
   begin
      return Check_Not_Null (S)                         -- # null
        and then S'Length > 0 and then S(S'First) = C;  -- # contents
   end;
end;
