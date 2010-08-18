function Starts (S : String; Key : String) return Boolean is
begin
   return Key'Length <= S'Length -- # startsLength
     and then S (S'First .. S'First + Key'Length - 1) = Key; -- # startsKey
end;

