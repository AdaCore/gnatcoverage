with Data;

package Global_Refs.Outrange is
   
   X : aliased Integer; -- # decl
   for X'Address use  -- # clause
     Data.Data_Address (Index => Data.Data_Array'Last + 1); -- # clause
   
end;
