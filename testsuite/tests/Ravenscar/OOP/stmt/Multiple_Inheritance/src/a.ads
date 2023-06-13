package A is
   type IA is interface;

   procedure P_IA_1 (X : in out IA) is null;
   procedure P_IA_2 (X : in out IA) is null;

   function Test_IA (X : IA) return Boolean is abstract;

   procedure Class_Wide_IA (X : in out IA'Class);
end A;
