with Ops; use Ops;

package Sys is
   type Op_Monitor_T is abstract tagged record
      N_Probes : Natural := 0;
   end record;
   procedure Probe (Mon : in out Op_Monitor_T) is abstract;

   type Unop_Monitor_T is new Op_Monitor_T with null record;
   procedure Probe (Mon : in out Unop_Monitor_T);

   type Binop_Monitor_T is new Op_Monitor_T with null record;
   procedure Probe (Mon : in out Binop_Monitor_T);

   function Process (A, B : Boolean; Op : Operator_T'Class) return Boolean;

end;
