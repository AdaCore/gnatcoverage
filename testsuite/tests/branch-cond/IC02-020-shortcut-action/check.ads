package Check is

   type T_Str (Size : Integer) is record
      Len : Integer;
      Tab : String (1 .. Size);
   end record;

   function Validate (This : T_Str; Str : String) return Boolean;
end;
