function Starts (S : String; Len : Integer; C : Character) return Boolean is
begin
   return Len <= S'Length  -- # startsLength
     and then S (S'First .. S'First + Len - 1) = (1 .. Len => C); -- # startsKey
end;

