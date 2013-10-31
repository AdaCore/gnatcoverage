with Data, System, Support;  use Support;

package body Process is
   
   procedure Check_Inrange is
      
      V : aliased Integer; -- # in_decl
      for V'Address use -- # in_clause
        Data.Data_Address(Index => Data.Data_Array'First); -- # in_clause
   
   begin
      Assert (V = Data.Data_Array(Data.Data_Array'First)); -- # in_stmt
   end;
   
   procedure Check_Outrange is
      
      V : aliased Integer; -- # out_decl
      for V'Address use -- # out_clause
        Data.Data_Address(Index => Data.Data_Array'Last + 1); -- # out_clause
      
      use type System.Address;
   begin
      Assert (V'Address = System.Null_Address); -- # out_stmt
   end;
   
end;

