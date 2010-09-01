package body Subprogram_Pack is

   function Fun1 (X  : T) return Integer is
   begin
      return X.I;            -- # fun1
   end Fun1;

   function Fun2 (X  : T) return Integer is
   begin
      return X.I + 1;        -- # fun2
   end Fun2;


   function Fun3 (X : T; I : Integer := Fun2 ((I => 1))) return Integer is
   begin
      return X.I + I;        -- # fun3
   end Fun3;

   procedure Proc1 (X : in out T) is
   begin
      X.I := X.I + 1;        -- # proc1
   end Proc1;

   procedure Proc2 (X : in out T) is
   begin
      X.I := X.I - 2;        -- # proc2
   end Proc2;

   procedure Class_Wide_Proc (X : in out T'Class) is
   begin
      Proc2 (X);             -- # class_wide
   end Class_Wide_Proc;

end Subprogram_Pack;
