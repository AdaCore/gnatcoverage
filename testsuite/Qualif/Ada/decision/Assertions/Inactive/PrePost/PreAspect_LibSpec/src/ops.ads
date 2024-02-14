pragma Ada_12;

with Optypes; use Optypes;

package Ops is

   pragma Assertion_Policy (Pre => Disable);

   type T_Operand is new T_Base_Operand with null record;

   procedure Apply (Op : T_Operation; A : in out T_Operand; B : T_Operand)
     with Pre => (A.Valid and then B.Valid); -- # cond
end;
