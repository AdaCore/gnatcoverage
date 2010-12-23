with Renamed_Objects; use Renamed_Objects;
package body Local_Renamings is

   procedure Get_Comp (Ind : Integer; Comp : out Integer) is
      X :  Integer renames Arr_Var (Get_Index (Ind));           -- # get_comp
   begin
      Comp := X;                                                -- # get_comp
   end Get_Comp;

   function Get_Another_Comp (Ind : Integer) return Integer is
      X :  Integer renames Another_Arr_Var (Get_Index (Ind));   -- # get_another_comp
   begin
      return X;                                                 -- # get_another_comp
   end Get_Another_Comp;

end Local_Renamings;
