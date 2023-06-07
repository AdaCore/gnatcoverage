with A; use A;
with B; use B;
package C is
   type IC is interface and IA and IB;

   function Test_IC (X : IC) return Boolean is abstract;

   procedure Class_Wide_IC (X : in out IC'Class);
end C;
