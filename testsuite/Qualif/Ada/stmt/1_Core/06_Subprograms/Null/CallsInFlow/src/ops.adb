
package body Ops is
   
   procedure Nop3 (X: in out Integer) is null; -- # do_nop3
   
   procedure Nop2 (X: in out Integer) is null; -- # do_nop2
      
   procedure Inc (X : in out Integer) is 
   begin
      X := X + 1; -- # do_inc
   end;     
   

   procedure Do_Pv1 (X : in out Integer; Opcode : T_Opcode) is
   begin
      if Opcode = Op_Nop1 then     -- # pv1_test_nop1
         Nop1 (X); -- # pv1_nop1
      elsif Opcode = Op_Nop2 then -- # pv1_test_nop2
         Nop2 (X); -- # pv1_nop2
      elsif Opcode = Op_Inc then -- # pv1_test_inc
         Inc (X);  -- # pv1_inc
      elsif Opcode = Op_Nop3 then -- # pv1_test_nop3
         Nop3 (X); -- # pv1_nop3
      end if;
   end;
   
   procedure Do_Pv2 (X : in out Integer; Opcode : T_Opcode) is
   begin
      case Opcode is -- # pv2_test
         when Op_Nop1 =>
            Nop1 (X);  -- # pv2_nop1
         when Op_Nop2 =>
            Nop2 (X);  -- # pv2_nop2
         when Op_Inc =>
            Inc (X);   -- # pv2_inc
         when Op_Nop3 =>
            Nop3 (X);  -- # pv2_nop3
      end case;
   end;
   
   procedure Process (X : in out Integer;
                      Opcode : T_Opcode;
                      Pv : T_Processing_Variant)
   is
   begin
      case Pv is -- # stmt
         when Pv1 => Do_Pv1 (X, Opcode); -- # pv1_call
         when Pv2 => Do_Pv2 (X, Opcode); -- # pv2_call
      end case;
   end;
      
end;
