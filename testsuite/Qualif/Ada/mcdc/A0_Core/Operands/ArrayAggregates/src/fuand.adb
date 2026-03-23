package body FUAND is

   function Invalid_Val_And_Loc (Op : Block) return Boolean is
   begin
      return Op.Val = ('n', 'u', 'l', 'l')  -- # evalA
        and then Op.Loc = (Pool'Range => Noloc_Id);  -- # evalB
   end;
end;
