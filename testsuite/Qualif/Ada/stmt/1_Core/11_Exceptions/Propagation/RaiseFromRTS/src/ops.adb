
package body Ops is
   
   procedure To_Integer
     (S : String; Value : out Integer; Fault : out Boolean)     
   is 
   begin
      Value := Integer'Value (S); -- # convert
      Fault := False; -- # no_fault
   exception
      when others =>
         Value := 0;    -- # fault
         Fault := True; -- # fault
   end;      
end;
