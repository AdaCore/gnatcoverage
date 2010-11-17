package Com is
   pragma Elaborate_Body;
   
   type State is (RX, TX, Idle);
   Current_State : State;
   
   Initialized : Boolean := False;   
   procedure Initialize;
   
   Auto_Initialize : Boolean := False;
   --  Variable to prevent obvious constant folding and absence of 
   --  code in conditioned sequences, which we test for coverage
end;

