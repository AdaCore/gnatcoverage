------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package body Ops is
   procedure Apply (Op : Op_Kind; X : in out Integer) is
   begin
      case Op is
         when Increment => X := X + 1;
         when Decrement => X := X - 1;
      end case;
   end Apply;
end Ops;
