
package body Vectors is
   
   function All_Same (N : Integer; Value : Integer) return Vector is
   begin
      return Vector'(1 .. N => Value);  -- # on-call
   end;
   
   function Sum_All_In (V : Vector) return Integer is
      Sum : Integer := 0;    -- # on-call
   begin
      for I in V'Range loop  -- # on-call
         Sum := Sum + V (I); -- # on-call
      end loop;
      return Sum;            -- # on-call
   end;
   
   function Sum_All_Abs (N : Integer; Value : Integer) return Integer is
   begin
      --  point here is to have conditional object declarations resorting to a
      --  function returning unconstrained for initializers
      
      if Value > 0 then -- # on-call
         declare
            V : Vector := All_Same (N, Value); -- # pos-decl
         begin
            return Sum_All_In (V); -- # pos-stmt
         end;
      else
         declare
            V : Vector := All_Same (N, -Value); -- # neg-decl
         begin
            return Sum_All_In (V); -- # neg-stmt
         end;
      end if;
   end;
   
end;
