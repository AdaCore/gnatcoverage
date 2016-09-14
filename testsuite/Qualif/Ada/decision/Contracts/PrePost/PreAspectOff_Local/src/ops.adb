pragma Ada_12;

package body Ops is
   
   pragma Assertion_Policy (Pre => Disable);
   
   procedure Apply (Op : T_Operation; A : in out T_Operand; B : T_Operand) is
     
     procedure Perform (Op : T_Operation; A : in out T_Operand; B : T_Operand)
       with Pre => (A.Valid and then B.Valid); -- # cond
   
     procedure Perform
       (Op : T_Operation; A : in out T_Operand; B : T_Operand) is
     begin
        case Op is
           when Op_Add => A.Value := A.Value + B.Value; -- # add
           when Op_Sub => A.Value := A.Value - B.Value; -- # sub
        end case;
     end;

   begin
      Perform (Op, A, B);
   end;
   
end;
