with Pack_Type_Invariant;

procedure Test_Type_Invariant
is
   use Pack_Type_Invariant;

   Dummy_1 : Bool_Record_1 := Make_Bool_Record_1 (True);
begin

   null;
end Test_Type_Invariant;

--# pack_type_invariant.ads
-- /type_dec_1/  l+ ## 0
-- /type_def_1/  l+ ## 0
-- /rec_1/       l+ ## 0
-- /type_inv_1/  l+ ## 0
-- /func_ret_1/  l+ ## 0
