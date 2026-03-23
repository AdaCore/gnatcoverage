package body FUOR is
   function Invalid_Val_Or_Loc (Op : Block) return Boolean is
   begin
      return Op.Val = ('n', 'u', 'l', 'l')  -- # evalA
        or else Op.Loc = (Pool'Range => Noloc_Id);  -- # evalB
   end;
end;
