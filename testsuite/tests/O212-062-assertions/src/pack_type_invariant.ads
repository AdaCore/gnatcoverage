pragma Ada_2012;
pragma Assertion_Policy (Check);

package Pack_Type_Invariant is

   type Bool_Record_1 is private;                                -- # type_dec_1

   function Make_Bool_Record_1 (B : Boolean) return Bool_Record_1;
private

   -- Type invariant as aspect
   type Bool_Record_1 is                                         -- # type_def_1
      record                                                     -- # rec_1
         B : Boolean := True;                                    -- # rec_1
      end record                                                 -- # rec_1
   with Type_Invariant => B;                                     -- # type_inv_1

   function Make_Bool_Record_1 (B : Boolean) return Bool_Record_1 is
      ((B => B));                                                -- # func_ret_1

end Pack_Type_Invariant;

