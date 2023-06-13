function Min (X, Y : Float) return Float is
   M : Float;
begin
   --  Compare X and Y
   if Y < X then
      M := Y;
   else
      M := X;
   end if;
   return M;
end Min;
