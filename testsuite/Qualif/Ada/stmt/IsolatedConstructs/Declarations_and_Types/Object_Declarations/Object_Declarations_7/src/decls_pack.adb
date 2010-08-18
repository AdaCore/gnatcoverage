package body Decls_Pack is

   procedure Local_1 (Res : in out Boolean) is
      Access_Integer_Var : Access_Integer;      -- # decl1
   begin
      Access_Integer_Var := new Integer'(1);    -- # code1
      Res := not Res;                           -- # code1
   end Local_1;

   function Local_2 (Arg : Boolean) return Boolean is
      Access_Const_Integer_Var : Access_Const_Integer; -- # decl2
   begin
      Access_Const_Integer_Var :=  new Integer'(1);    -- # code2
      return not Arg;                                  -- # code2
   end Local_2;

end Decls_Pack;
