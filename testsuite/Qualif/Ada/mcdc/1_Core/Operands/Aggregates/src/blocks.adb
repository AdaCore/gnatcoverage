package body Blocks is

   function Terminator (Blk : Block) return Boolean is
   begin
      return Blk = ('n', 'u', 'l', 'l')  -- # evalA
        or else Blk = (Blk'Range => 'x');  -- # evalB
   end;
end;
