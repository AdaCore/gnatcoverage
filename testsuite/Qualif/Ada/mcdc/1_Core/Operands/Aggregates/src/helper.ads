package Helper is

   --  Helper package to invoke the functional decision in specific ways, with
   --  known values for the to-be-evaluated conditions. This simplifies drivers
   --  and is not subject to coverage analysis.

   --  Variations over individual conditions

   procedure Eval_FF_F;
   procedure Eval_FT_T;
   procedure Eval_TF_T;
   procedure Eval_TT_T;

end;
