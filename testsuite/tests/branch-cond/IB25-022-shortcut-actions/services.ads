package Services is
   type Str_Type (Size : Integer) is record
      Value : String (1 .. Size);
   end record;

   procedure Check_Value_And (This : in out Str_Type);
   procedure Check_Value_Or (This : in out Str_Type);
   procedure Check_Addr_And (This : Str_Type);
   procedure Check_Addr_Or (This : Str_Type);
end;

