with Optypes; use Optypes;

package Ops is
   
   type T_Operand is new T_Base_Operand with null record;
   
   procedure Apply (Op : T_Operation; A : in out T_Operand; B : T_Operand);
end;
