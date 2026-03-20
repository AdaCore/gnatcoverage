-- Common functional code that will get to be called from various contexts

package body Colors is

   function Code_For (Colv : Color) return Code is
   begin
      case Colv is -- # case
         when Red   => return R; -- # red
         when Green => return G; -- # green
         when Blue  => return B; -- # blue
      end case;
   end;

end;
