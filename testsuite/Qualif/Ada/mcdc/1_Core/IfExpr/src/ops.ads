--  A simple OO hierarchy to be used by real tests. This is just meant
--  to allow some amount of sophistication in the constructs exercised
--  by the tests, so they can manipulate more elaborate entities than mere
--  Boolean variables or Integer values, perform membership tests, ...
--
--  We never assess the coverage of this code.

package Ops is
   type Operator_T is abstract tagged null record;

   type Unary_Operator_T is abstract new Operator_T with null record;
   function Eval (Op : Unary_Operator_T; B : Boolean) return Boolean
     is abstract;

   type Op_Not_T is new Unary_Operator_T with null record;
   function Eval (Op : Op_Not_T; B : Boolean) return Boolean;

   type Op_Self_T is new Unary_Operator_T with null record;
   function Eval (Op : Op_Self_T; B : Boolean) return Boolean;

   type Binary_Operator_T is abstract new Operator_T with null record;
   function Eval (Op : Binary_Operator_T; A, B : Boolean) return Boolean
     is abstract;

   type Op_And_T is new Binary_Operator_T with null record;
   function Eval (Op : Op_And_T; A, B : Boolean) return Boolean;

end;
