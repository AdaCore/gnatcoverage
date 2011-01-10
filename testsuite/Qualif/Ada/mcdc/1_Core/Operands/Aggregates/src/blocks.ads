package Blocks is

   -- fake model of a memory block allocator, where blocks are
   -- represented as strings known to be at least 4 characters long

   subtype Block is String;

   function Terminator (Blk : Block) return Boolean;
end;
