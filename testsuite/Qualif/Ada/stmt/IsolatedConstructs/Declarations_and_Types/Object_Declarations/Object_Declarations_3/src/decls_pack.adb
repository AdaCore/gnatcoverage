package body Decls_Pack is

   procedure Local_1 (Res : in out Boolean) is
      Coordinate_V : Coordinate := Coordinate_Zero;  -- # decl1
   begin
      Res := not Res;                                -- # code1
   end Local_1;

   function Local_2 (Arg : Boolean) return Boolean is
      My_String : Var_String := (Len => 3, Data => "Ada");    -- # decl2
   begin
      return not Arg;                                         -- # code2
   end Local_2;

end Decls_Pack;
