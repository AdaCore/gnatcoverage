function Starts (S : String; Key : String) return Boolean is
begin
   return Key'Length = 3
     and then S (S'First .. S'First + Key'Length) = "abc"; -- # contents
end;
