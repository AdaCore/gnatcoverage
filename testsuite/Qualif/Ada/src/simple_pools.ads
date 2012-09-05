--  This unit offers very simple allocator services to prevent dependency
--  against a generic malloc like memory allocation service that the most
--  basic runtime profiles might not feature. This is intended for simple
--  tests, never deallocating and never needing more than a few nodes.
   
package Simple_Pools is
   
   generic 
      type Data_Type is limited private;
      Capacity : Natural;
   package Basic_Pool is
      type Data_Access is access all Data_Type;
      function Allocate return Data_Access;
   end;
   
end Simple_Pools;
