package B is
   type IB is interface;

   procedure P_IB_1 (X : in out IB) is null;
   procedure P_IB_2 (X : in out IB) is null;

   function Test_IB (X : IB) return Boolean is abstract;

   procedure Class_Wide_IB (X : in out IB'Class);
end B;
