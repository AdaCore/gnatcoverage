package body Counters is

   procedure Step_By (N : Natural; X : in out Integer; Op : OpKind) is
   begin
      case Op is        -- # case
         when Inc =>
            X := X + N; -- # opInc
         when Dec =>
            X := X - N; -- # opDec
      end case;
   end;
end;
