package Ops is
   procedure Div (X, Y : Integer; T : out Integer; Fault : out Boolean);
   procedure Bump (X : in out Integer; Fault : out Boolean);
end;
