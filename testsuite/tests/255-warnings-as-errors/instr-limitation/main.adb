procedure Main is
   package Inner is
      type T is tagged null record;

      function Constructor return T is (T'(null record));
   end Inner;
begin
   null;
end Main;
