package body Parent is

   function Compute_C (X : T'Class) return Integer is
   begin
      if Valid (X) then        -- # compute_c_eval
         return Compute (X);   -- # compute_c_true
      else
         return 0;             -- # compute_c_false
      end if;
   end Compute_C;

   function Valid_C (X : T'Class) return Boolean is
   begin
      return Valid (X);        -- # valid_c
   end Valid_C;

end Parent;
