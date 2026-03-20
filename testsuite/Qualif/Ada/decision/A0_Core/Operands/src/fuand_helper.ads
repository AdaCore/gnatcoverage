package FUAND_Helper is

   --  Helper package to invoke a functional and-then decision in specific
   --  ways, with known values for the to-be-evaluated conditions.

   --  This simplifies drivers and is not subject to coverage analysis.

   procedure Eval_FX_F;
   procedure Eval_TF_F;
   procedure Eval_TT_T;

end;
