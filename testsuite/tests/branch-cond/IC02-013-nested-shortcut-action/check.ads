package Check is

   type T_Str (Size : Integer) is record
      Value : String (1 .. Size);
   end record;

   function Valid (S : T_Str; X, Y : Integer) return Boolean;
end;
