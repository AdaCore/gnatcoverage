package Ops is
   procedure Nop1 (X: in out Integer) is null; -- # do_nop1
   
   procedure Nop2 (X : in out Integer);
   
   procedure Inc (X : in out Integer);
   
   type T_Opcode is (Op_Nop1, Op_Nop2, Op_Nop3, Op_Inc);
   type T_Processing_Variant is (PV1, PV2);
   
   procedure Process
     (X : in out Integer; Opcode : T_Opcode; Pv : T_Processing_Variant);
end;

