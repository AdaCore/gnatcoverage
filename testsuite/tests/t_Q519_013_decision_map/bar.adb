package body Bar is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return (A and then B) or else C;
   end F;
end Bar;
