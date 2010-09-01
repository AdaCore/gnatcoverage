package Subprogram_Pack is

   type T is tagged record
      I : Integer;
   end record;

   function Fun1 (X  : T) return Integer;
   function Fun2 (X  : T) return Integer;

   function Fun3 (X : T; I : Integer := Fun2 ((I => 1))) return Integer;

   procedure Proc1 (X : in out T);
   procedure Proc2 (X : in out T);

   procedure Class_Wide_Proc (X : in out T'Class);

end Subprogram_Pack;
