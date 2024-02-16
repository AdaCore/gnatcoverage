package Misc is

   function Aligned (X, Y, Factor : Integer) return Boolean;
   pragma Import (C, Aligned, "aligned");
end;
