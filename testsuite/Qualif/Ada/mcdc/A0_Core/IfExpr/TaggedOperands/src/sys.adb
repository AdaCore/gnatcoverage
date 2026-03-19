pragma Ada_2012;

package body Sys is

   type mon_Access is access all OP_Monitor_T'Class;

   Umon : aliased Unop_Monitor_T;
   Bmon : aliased Binop_Monitor_T;

   Uprobes, Bprobes : Natural := 0;

   procedure Probe (Mon : in out Unop_Monitor_T) is
   begin
      Uprobes := Uprobes + 1; -- # umon
   end;

   procedure Probe (Mon : in out Binop_Monitor_T) is
   begin
      Bprobes := Bprobes + 1; -- # bmon
   end;

   function Umon_Ptr return Mon_Access is
   begin
      return Umon'Access; -- # umon
   end;

   function Bmon_Ptr return Mon_Access is
   begin
      return Bmon'Access; -- # bmon
   end;

   function Process (A, B : Boolean; Op : Operator_T'Class) return Boolean
   is
      Mon : Op_Monitor_T'Class :=
	(if Op in Unary_Operator_T'Class then Umon_Ptr.all -- # unop
	else Bmon_Ptr.all);
   begin
      Probe (Mon);
      return (if Op in Binary_Operator_T'Class then -- # binop
	Binary_Operator_T'Class(Op).Eval (A, B)
	else Unary_Operator_T'Class(Op).Eval (A));
   end;
end;
