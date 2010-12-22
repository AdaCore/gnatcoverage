package Renamed_Objects is

   type Arr is array (1 .. 10) of Integer;
   Arr_Var         : Arr;
   Another_Arr_Var : Arr;

   function Get_Index (I : Integer) return Integer;
end Renamed_Objects;
