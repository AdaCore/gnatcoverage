package body Starts is
   function Starts3 (S : String_Access; C : Character) return Boolean is
   begin
      return S /= null                                  -- # null
        and then S'Length > 0 and then S(S'First) = C;  -- # contents
   end;
end;
