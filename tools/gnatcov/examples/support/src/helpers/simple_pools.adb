package body Simple_Pools is
   
   package body Basic_Pool is
      
      Store : array (1 .. Capacity) of aliased Data_Type;
      Next_Free : Natural := Store'First;
      
      function Allocate return Data_Access is
         This_Index : constant Natural := Next_Free;
      begin
         Next_Free := Next_Free + 1;
         return Store (This_Index)'Access;
      end;      

   end;
   
end;
