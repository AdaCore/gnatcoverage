separate (Lib)
function Mult (Left, Right : Positive) return Positive is
begin
   case Left is
      when 1 =>
         case Right is
            when 1 =>
               return 1;
            when others =>
               null;
         end case;
      when others =>
         null;
   end case;

   raise Program_Error;
   return 1;
end Mult;
