package Fatp is
   type String_Access is access all String;
   function Is_Null (S : String_Access) return Boolean;
end;
