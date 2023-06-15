package body Parent.Child1 is

   function Compute (X : T1_T) return Integer is
   begin
      if Valid (T1_T'Class (X)) then   -- # compute_eval
         return X.C1;                  -- # compute_true
      else
         return 0;                     -- # compute_false
      end if;
   end Compute;

   function Valid (X : T1_T) return Boolean is
   begin
      if X.C1 in -10 .. 10 then        -- # valid_if_eval
         return True;                  -- # valid_if_true
      elsif X.C1 in -100 .. 100 then   -- # valid_elsif_eval
         return False;                 -- # valid_elsif_ true
      else
         raise Constraint_Error;       -- # valid_false
      end if;
   end Valid;

end Parent.Child1;
