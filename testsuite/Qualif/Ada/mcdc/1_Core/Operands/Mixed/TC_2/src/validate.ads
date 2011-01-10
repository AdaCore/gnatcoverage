package Validate is

   type Str (Max_Size : Integer) is record
      Length : Integer;
      Value  : String (1 .. Max_Size);
   end record;
   type Str_Access is access all Str;

   function Valid (Sptr : Str_Access) return Boolean;

   Valid_Str   : constant Str_Access :=
     new Str'(Max_Size => 5, Length => 5 , Value => "VALID");
   Toolong_Str : constant Str_Access :=
     new Str'(Max_Size => 10, Length => 10, Value => "VALID56789");
   Invalid_Str : constant Str_Access :=
     new Str'(Max_Size => 5, Length => 5 , Value => "01234");
end;
