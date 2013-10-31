with Data;

package Global_Refs.Inrange is
   
   X : aliased Integer; -- # decl
   for X'Address use  -- # clause
     Data.Data_Address (Index => Data.Data_Array'First); -- # clause
   
end;
