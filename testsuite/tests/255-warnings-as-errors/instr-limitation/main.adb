procedure Main is
   package Inner is
      type T is tagged null record;

      T_Val : constant T := (null record);

      function Constructor return T is (T_Val'Unrestricted_Access.all);
   end Inner;
begin
   null;
end Main;
