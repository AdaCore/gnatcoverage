pragma Extensions_Allowed (On);

generic
   with function F (B : Boolean) return Boolean is (B);  -- # f
function Eval_F (B : Boolean) return Boolean;
