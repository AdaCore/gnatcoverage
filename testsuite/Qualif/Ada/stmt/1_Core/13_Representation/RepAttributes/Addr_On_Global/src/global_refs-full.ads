with Data;

package Global_Refs.Full is
   
   X : aliased Integer; -- # decl
   for X'Address use  -- # clause
     Data.Data_Address (Index => Data.Data_Array'First); -- # clause
   
   Y : aliased Integer; -- # decl
   for Y'Address use  -- # clause
     Data.Data_Address (Index => Data.Data_Array'Last+1); -- # clause
   
end;
