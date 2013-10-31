
package body Data is
   function Data_Address (Index : Integer) return System.Address is
   begin
      if Index not in Data_Array'Range then -- # test_index
         return System.Null_Address;        -- # out_range
      else
         return Data_Array(Index)'Address;  -- # in_range
      end if;
   end;
end;
  
