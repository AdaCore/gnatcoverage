package Starts is
   type String_Access is access all String;
   function Starts3 (S : String_Access; C : Character) return Boolean;
end;
