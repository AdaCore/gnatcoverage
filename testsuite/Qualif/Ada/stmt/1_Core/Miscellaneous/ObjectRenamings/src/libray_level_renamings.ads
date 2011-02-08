with Renamed_Objects; use Renamed_Objects;
package Libray_Level_Renamings is
   Renamed_Comp_1 : Integer renames Arr_Var (Get_Index (1));  -- # renaming
end Libray_Level_Renamings;
