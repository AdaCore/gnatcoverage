pragma Extensions_Allowed (On);

generic
   with function F (B : Boolean) return Boolean is (B);      -- # f
package Pkg is
   function Eval_F (B : Boolean) return Boolean is (F (B));  -- # eval_f
end Pkg;
