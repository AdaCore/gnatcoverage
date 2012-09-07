package body Validate is
   function Length (S : Str) return Integer is
   begin
      return S.Length;  -- # retLen
   end;

   function Valid (Sptr : Str) return Boolean is
   begin
      return (Length (Sptr) = 5 -- # checkLen
                and then Sptr.Value (1 .. 5) = "VALID");  -- # checkKey
   end;
end;
