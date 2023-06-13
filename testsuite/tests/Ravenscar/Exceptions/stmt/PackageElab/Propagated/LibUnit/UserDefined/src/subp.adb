package body SubP is

   function func (x: integer) return integer is
   -- x must be in range 1 thru 10
   begin
      if x < 1 or x >=10 then                   -- # test
         raise My_Error;                        -- # explicit_violation
      else
         return x;                              -- # no_exp_violation
      end if;
   end func;

end SubP;
