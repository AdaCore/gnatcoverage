package body Inlined_Subprograms is
   procedure Proc_No_Inline  (I : in out Integer) is
   begin
      Proc1 (I);                      -- # proc_no_inline
   end Proc_No_Inline;

   procedure Proc1 (I : in out Integer) is
   begin
      I := Fun1 (I);                  -- # proc1
   end Proc1;

   procedure Proc2 (I : in out Integer) is
   begin
      I := I + 1;                     -- # proc2
   end Proc2;

   function Fun_No_Inline (I : Integer) return Integer is
   begin
      return Fun2 (I);                -- # fun_no_inline
   end Fun_No_Inline;

   function Fun1 (I : Integer) return Integer is
   begin
      return I + 100;                 -- # fun1
   end Fun1;

   function Fun2 (I : Integer) return Integer is
      Res : Integer := I;             -- # fun2
   begin
      Proc2 (Res);                    -- # fun2
      return Res;                     -- # fun2
   end Fun2;
end Inlined_Subprograms;
