package body Pkg is

   function Or_Else (L, R : Boolean) return Boolean is
   begin
      return L or else R;
   end Or_Else;

end Pkg;
