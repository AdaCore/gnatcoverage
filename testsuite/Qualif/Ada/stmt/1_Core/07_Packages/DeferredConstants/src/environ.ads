package Environ is
   
   type State is private;
   
   function Probe return State;
   
   Did_Initial_Probe, Did_Other_probe : Boolean := False;
   
   function Match (A, B : State) return Boolean;
   
private
   type Value is mod 2;
   type State is record
      X : Value;
   end record;
end;

