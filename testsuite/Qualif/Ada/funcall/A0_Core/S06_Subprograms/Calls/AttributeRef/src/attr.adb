pragma Ada_2012;

function Attr (S : String) return String is    -- # fun

   function Id return String is (S);           -- # id

begin
   return Integer'(Id'Length)'Image;           -- # attr
end Attr;
