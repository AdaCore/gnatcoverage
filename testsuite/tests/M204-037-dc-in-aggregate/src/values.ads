package Values is
   
   type Bool is record
      Value : Boolean;
   end record;
   
   procedure Equal (X, Y : Integer; B : out Bool);
   procedure Equal2 (X, Y : Integer; Z, T : Integer; B : out Bool);
   
end;
