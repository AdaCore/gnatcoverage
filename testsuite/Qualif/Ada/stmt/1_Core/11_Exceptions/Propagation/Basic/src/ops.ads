package Ops is
   procedure Div (X, Y : Integer; T : out Integer; Fault : out Boolean);
   
   N_Faults : Integer := 0;
end;
