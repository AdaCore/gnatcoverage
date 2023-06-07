package body Multiop is
   
   procedure Set (I : in out Int; V : Integer) is
   begin
      I.Value := V;
   end;
end;
