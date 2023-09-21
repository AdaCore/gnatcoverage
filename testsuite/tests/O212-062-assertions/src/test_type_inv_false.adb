with Pack_Type_Invariant;

with Ada.Assertions;

procedure Test_False
is
   use Pack_Type_Invariant;

begin
   begin
      declare
         Dummy_1 : constant Bool_Record_1 := Make_Bool_Record_1 (False);
      begin
         null;
      end;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Test_False;

--# pack_type_invariant.ads
-- /type_dec_1/  l+ ## 0
-- /type_def_1/  l+ ## 0
-- /type_inv_1/  l! ## aT-
-- /func_ret_1/  l+ ## 0
