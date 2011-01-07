
package Services is   
   type String_Access is access all String;
   function Starts (S : String_Access; C : Character) return Boolean;
end;
