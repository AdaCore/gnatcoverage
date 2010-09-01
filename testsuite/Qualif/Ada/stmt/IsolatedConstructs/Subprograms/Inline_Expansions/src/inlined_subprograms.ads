package Inlined_Subprograms is
   procedure Proc_No_Inline  (I : in out Integer);
   procedure Proc1           (I : in out Integer);
   procedure Proc2           (I : in out Integer);
   pragma Inline (Proc1, Proc2);

   function Fun_No_Inline  (I : Integer) return Integer;
   function Fun1           (I : Integer) return Integer;
   function Fun2           (I : Integer) return Integer;
   pragma Inline (Fun1, Fun2);
end Inlined_Subprograms;
