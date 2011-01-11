package body FUOR is

   function Invalid (Op : Block) return Boolean is
   begin
      --  or else with operands featuring aggregates

      return Op.Val = ('n', 'u', 'l', 'l')  -- # evalA
        or else Op.Loc = (Pool'Range => Noloc_Id);  -- # evalB
   end;
end;
