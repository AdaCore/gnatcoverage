package body Ops is
   
   procedure Apply (Op : T_Operation; A : in out T_Operand; B : T_Operand) is
   begin
      if Op = Op_Add then -- # test_add
         A.Value := A.Value + B.Value; -- # add
      elsif Op = Op_Sub then -- # test_sub
         A.Value := A.Value - B.Value; -- # sub
      end if;
   end;
   
end;
