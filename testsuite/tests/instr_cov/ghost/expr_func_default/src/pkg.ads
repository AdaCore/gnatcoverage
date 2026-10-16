pragma Extensions_Allowed (On);

generic
   with function Predicate (I : Integer) return Boolean
   is (I >= 0)                -- # expr_func_default
   with Ghost;
package Pkg is
   procedure Print (I : Integer)
   with Pre => Predicate (I); -- # precond
end Pkg;
